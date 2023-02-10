#!/bin/bash

LOGFILE="test.log"
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

shopt -s extglob

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
    exec 4<&1 5<&2 1>&2>&>(tee >(sed -r 's/\x1B\[([0-9]{1,2}(;[0-9]{1,2})*)?[m|K]//g' > $1.tmpresult))

    expected_exit_code=0
    if grep -q '# Exit:' $1; then 
        expected_exit_code=$(awk '/# Exit:/{getline; print}' $1 | grep -o '[0-9]\+')
    fi 
    
    # Account for invalid exit codes
    if [ "$expected_exit_code" -ne 100 ] && [ "$expected_exit_code" -ne 200 ]; then
        expected_exit_code=0
    fi

    output=$(./compile "$1")
    result_exit_code=$?

    if [ $expected_exit_code -eq $result_exit_code ]; then
        echo -e "\e[1;32m[PASS]\e[0m \e[32m$file\e[0m"
    else
        echo -e "\e[1;31m[FAIL]\e[0m \e[31m$file\t expected: $expected_exit_code but got: $result_exit_code\e[0m"
    fi

    echo "$output"
    echo

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
    echo -e "\e[1;31m[FAIL]\e[0m \e[31m$(basename $file)\e[0m"
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
