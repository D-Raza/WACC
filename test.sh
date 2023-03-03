#!/bin/bash

shopt -s extglob

LOGFILE="test.log"
EXECDIR="test_exec"
NO_THREADS=4

# Usage message
if [[ ( $@ == "--help") ||  $@ == "-h" ]]
then 
    echo "Usage: $0 [dir: wacc_examples]"
	exit 0
fi 

# Retrieve the latest wacc_examples
if [ ! -d wacc_examples ]; then
  git clone https://gitlab.doc.ic.ac.uk/lab2223_spring/wacc_examples.git/ --quiet
else
  cd wacc_examples
  git reset --hard HEAD --quiet
  git pull --quiet
  cd ..
fi

if [ ! -d "$EXECDIR" ]; then
    mkdir "$EXECDIR"
else
    rm -rf "$EXECDIR/*"
fi
rm -f *.s

# Find all .wacc files in directory
dir=${1:-wacc_examples}
if ! files=$(find $dir -type f -name "*.wacc"); then
    exit 1
fi

# Get the total number of test cases
no_files=$(echo "$files" | wc -l)
if [ $no_files -lt 2 ]; then
    echo -e "\e[1;31m[ERROR] No test cases found in $dir.\e[0m"
    exit 1
fi



echo -e "\e[1;35m-----\e[0m \e[1;4;35mWACC Tests\e[0m \e[1;35m-----\e[0m" | tee -a >(sed -r 's/\x1B\[([0-9]{1,2}(;[0-9]{1,2})*)?[m|K]//g' > $LOGFILE)
echo -e "\e[1;35mRunning $no_files tests in $dir\e[0m" | tee -a >(sed -r 's/\x1B\[([0-9]{1,2}(;[0-9]{1,2})*)?[m|K]//g' > $LOGFILE)

# Test a file
test_task() {
    file=$1
    exec 4<&1 5<&2 1>&2>&>(tee >(sed -r 's/\x1B\[([0-9]{1,2}(;[0-9]{1,2})*)?[m|K]//g' > $file.tmpresult))

    expected_runtime_exit_code=0
    if grep -q '# Exit:' $file; then 
        expected_runtime_exit_code=$(awk '/# Exit:/{getline; print}' $file | grep -o '[0-9]\+')
    fi

    expected_output=""
    if grep -q '# Output:' $file; then 
        expected_output=$(awk '/^# Output/{getline; p=1} p && /^#/ {print substr($0, 3); next} /^$/ {p=0}' $file | sed -e '/Program:/d' | awk 'NR>1{print PREV} {PREV=$0} END{printf("%s",$0)}'; r=$?; echo /; exit "$r")
        expected_output=${expected_output%/}
    fi

    std_input=""
    if grep -q '# Input:' $file; then 
        std_input=$(awk -F ': ' '/^# Input:/ {print $2; exit}' $file)
    fi
    
    # Account for invalid exit codes
    if [ "$expected_runtime_exit_code" -ne 100 ] && [ "$expected_runtime_exit_code" -ne 200 ]; then
        expected_compiler_exit_code=0
    else
        expected_compiler_exit_code=$expected_runtime_exit_code
    fi
    
    compiler_output=$(./compile "$file")
    compiler_exit_code=$?

    if [ $expected_compiler_exit_code -eq $compiler_exit_code ]; then
        if [ $compiler_exit_code -eq 0 ]; then
            sleep 0.1
            filename=$(basename -- "$file")
            filename="${filename%.*}"
            outfile="$filename.s"

            if [ -f "$outfile" ]; then
                assembler_output=$(arm-linux-gnueabi-gcc -o "$EXECDIR/$filename" -mcpu=arm1176jzf-s -mtune=arm1176jzf-s "$filename.s" 2>&1)
                assembler_exit_code=$?
                if [ $assembler_exit_code -eq 0 ]; then
                    if [ ! -z "$std_input" ]; then
                        emulator_output=$(echo "$std_input" | qemu-arm -L /usr/arm-linux-gnueabi/ "$EXECDIR/$filename"; r=$?; echo /; exit "$r")
                    else
                        emulator_output=$(qemu-arm -L /usr/arm-linux-gnueabi/ "$EXECDIR/$filename"; r=$?; echo /; exit "$r")
                    fi
                    emulator_exit_code=$?
                    emulator_output=${emulator_output%/}
                    if [ $emulator_exit_code -eq $expected_runtime_exit_code ]; then
                        if [ ! -z "$expected_output" ]; then
                            output_diff=$(diff -b <(echo "$expected_output" | grep -vE '#addrs#|#runtime_error#') <(echo "$emulator_output" | grep -vE '0x|fatal'))
                            # if [ "${emulator_output}" == "${expected_output}" ]; then
                            if [ -z "$output_diff" ]; then
                                test_result="\e[1;32m[PASS]\e[0m \e[32m$file\e[0m"
                            else
                                test_result="\e[1;31m[FAIL]\e[0m \e[31m$file\t BACKEND - incorrect output at runtime\e[0m"
                            fi
                        else
                            test_result="\e[1;32m[PASS]\e[0m \e[32m$file\e[0m"
                        fi
                    else
                        test_result="\e[1;31m[FAIL]\e[0m \e[31m$file\t BACKEND - expected: $expected_runtime_exit_code but got: $emulator_exit_code\e[0m"
                    fi
                else
                    test_result="\e[1;31m[FAIL]\e[0m \e[31m$file\t BACKEND - assembly of $outfile unsuccessful\e[0m"
                fi
            else
                test_result="\e[1;31m[FAIL]\e[0m \e[31m$file\t BACKEND - $outfile not found\e[0m"
            fi
        else
            test_result="\e[1;32m[PASS]\e[0m \e[32m$file\e[0m"
        fi
    else
        test_result="\e[1;31m[FAIL]\e[0m \e[31m$file\t FRONTEND - expected: $expected_compiler_exit_code but got: $compiler_exit_code\e[0m"
    fi

    echo -e "$test_result"

    if [ $compiler_exit_code -ne 0 ]; then
        echo -e "\e[34m--- Compiler Output ---\e[0m"
        echo "$compiler_output"
        echo
    fi

    if [ $assembler_exit_code -ne 0 ]; then
        echo -e "\e[34m--- Assembler Output ---\e[0m"
        echo "$assembler_output"
        echo
    fi

    # if [ "$emulator_output" != "$expected_output" ]; then
    if [ ! -z "$output_diff" ]; then
        echo -e "\e[34m--- Emulator Output Difference ---\e[0m"
        echo "$output_diff"
        echo
    fi
  
    exit 0
}

for file in $files
do
  (test_task $file 2>&1) &

  if [[ $(jobs -r -p | wc -l) -ge $NO_THREADS ]]; then
      wait -n
  fi
done
wait

no_errors=0
for file in $files
do
  res_file=$file.tmpresult
  if grep -q '\[FAIL\]' $res_file; then
    no_errors=$((++no_errors))
    echo -e "\e[1;31m[FAIL]\e[0m \e[31m$file\e[0m"
  fi
  cat $res_file | tee -a $LOGFILE >> /dev/null
  rm $res_file
done

# Print out summary
if [ "$no_errors" -gt 0 ]; then
  echo -e "\e[1;31m$no_errors test case(s) failed out of $no_files (see $LOGFILE for more details)\e[0m"
  exit 1
else
  echo -e "\e[42mPassed all test cases!\e[0m"
  exit 0
fi
