# 3494. Find the Minimum Amount of Time to Brew Potions

## Problem Description

You are given two arrays: `skill` representing the skill levels of wizards and `mana` representing the mana costs for brewing potions. The goal is to find the minimum amount of time required to brew all potions, considering the constraints of wizard skills and potion mana requirements.

## Solution Approach

This solution uses dynamic programming to calculate the minimum time. It iterates through the potions and wizards, keeping track of the previous completion times to optimize the brewing schedule.

## Time Complexity

O(n * m), where n is the number of wizards and m is the number of potions.

## Space Complexity

O(1), excluding the input arrays.

## Usage

Compile and run the C++ code with your preferred compiler. The `minTime` function takes two vectors: `skill` and `mana`, and returns the minimum time as a `long long`.

