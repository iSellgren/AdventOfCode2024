#!/bin/bash
input_file="input/Day3.txt"
output_file="input/Day3_cheat.txt"
> "$output_file"
grep -o 'mul([0-9]\+,[0-9]\+)' "$input_file" | sed -e 's/mul(//;s/)//' > "$output_file"

output_file_part2="input/Day3_part2_input.txt"
> "$output_file_part2"
grep -o 'mul([0-9]\+,[0-9]\+)\|do()\|don.t()' "$input_file" > "$output_file_part2"
