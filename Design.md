# Panicscript

## Introduction

Panicscript is an imperative programming language with a focus on simplicity, transparency, and explicit intention.
Syntax is meant to be straight forward and simple with very little hidden from the programmer.

## Design

### Core
- [] Int
- [] Boolean
- [] Variables
- [] 'State' Control/Loop Structure
- [] Functions

These features were chosen for core because they satisfy most commonly used programming tasks.

The 'State' control structure is a unique take on case or switch statements in other languages. State accepts an
arbitrary list of expressions that return boolean values. These expressions are individually evaluated and a binary
number is returned based on the result. This binary is used in the labels to decide which path execution should take.
State also provides a looping construct. When a label is matched, an action keyword to determine behavior can be given
at the end of the block.
Available keywords are:

- Reval			- Loops back to state and reevaluates expressions passed to State
- Break			- Escapes State construct
- Go [label]	- Jumps to label specified

Omission of action keyword causes fall through to next label.

Labels provided do not need to be exhaustive of all possible binary combinations. If the return does not match a label,
the Default label is executed. If there is no Default label and the return has no label, execution continues.

An example of the State control/loop structure,

	x = 1
	y = 3
	State(x <= 2, x > y):
		00:
			y--
			Reval
		01:
			Go 11:
		10:
			x++
			Reval
		11:
			Break
		Default:
			x = y

The flow would happen as follows:

	((x=1) < 2, (x=1) > (y=3)) returns: 10
		10: x++ Reval
	((x=2) < 2, (x=2) > (y=3)) returns: 10
		10: x++ Reval
	((x=3) < 2, (x=3) > (y=3)) returns: 00
		00: y-- Reval
	((x=3) < 2, (x=3) > (y=2)) returns: 01
		01: Go 11:
		11: Break
	Execution continues with x=3, y=2

'for' or 'while' loops can be implemented with the State Structure,

	i = 0
	x = 0
	State(i < 5):
		1:
			x = x + 2
			i++
			Reval

As can if statements,

	x = 3
	y = 5
	State(x < y):
		1:
			x = y

Or if-else statements,

	x = 3
	y = 5
	State(x > y):
		1:
			x = y
			Break
		0:
			y = x


### Syntactic Sugar
- [] Increment/Decrement
- [] Ternary Statements
