#!/bin/bash

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
  git pull --quiet
  cd ..
fi

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

# Write output to test.log
exec 4<&1 5<&2 1>&2>&>(tee -a >(sed -r 's/\x1B\[([0-9]{1,2}(;[0-9]{1,2})*)?[m|K]//g' > test.log))

echo -e "\e[1;35m-----\e[0m \e[1;4;35mWACC Tests\e[0m \e[1;35m-----\e[0m"
echo -e "\e[1;35mRunning tests in $dir:\e[0m"

# Test each file
no_errors=0
for file in $files
do
    expected_exit_code=0
    if grep -q '# Exit:' $file; then 
        expected_exit_code=$(awk '/# Exit:/{getline; print}' $file | grep -o '[0-9]\+')
    fi 

    output=$(./compile "$file")
    result_exit_code=$?

    if [ $expected_exit_code -eq $result_exit_code ]; then
        echo -e "  \e[1;32m[PASS]\e[0m \e[32m$file\e[0m"
    else
        echo -e "  \e[1;31m[FAIL]\e[0m \e[31m$file\t expected: $expected_exit_code but got: $result_exit_code\e[0m"
        no_errors=$((++no_errors))
    fi
done

# Print out summary
echo
if [ "$no_errors" -gt 0 ]; then
  echo -e "\e[1;31m$no_errors test cases failed out of $no_files.\e[0m"
  exit 1
else
  echo -e "\e[42mPassed all test cases!\e[0m"
  exit 0
fi
