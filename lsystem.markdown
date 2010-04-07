---
title: L-System gallery 
---

## A bushy tree

Rule:

    F -> FF-[-F+F+F]+[+F-F-F]

Interpretation:

    Start symbol = F
    F = forward 5
    + = right 25 
    - = left 25
    [ = push
    ] = pop

Rendering after 5 iterations:

<div align="center">
![]($root/images/bushy.jpg)
</div>

## A sticky tree

Rules:

    X -> F-[[X]+X]+F[+FX]-X
    F -> FF

Interpretation:

    Start symbol = X 
    F = forward 2
    + = right 25 
    - = left 25
    [ = push
    ] = pop

Rendering after 7 iterations:

<div align="center">
![]($root/images/sticky.jpg)
</div>

## A Sierpinski triangle

Rules:

    A -> B-A-B
    B -> A+B+A

Interpretation:

    Start symbol = A 
    A = forward 0.5 
    B = forward 0.5 
    + = left 60 
    - = right 60 
    [ = push
    ] = pop

Rendering after 10 iterations:

<div align="center">
![]($root/images/sierpinski.jpg)
</div>

## A Peano Gosper curve

Rules:

    X -> X+YF++YF-FX--FXFX-YF+
    Y -> -FX+YFYF++YF+FX--FX-Y
    F -> F

Interpretation:

    Start symbol = X 
    F = forward 4
    + = right 60 
    - = left 60 
    [ = push
    ] = pop

Rendering after 5 iterations:

<div align="center">
![]($root/images/peano_gosper.jpg)
</div>

## A fractal crystal

Rules:

	I -> F+F+F+F
	F -> FF+F++F+F

Interpretation:

	Start symbol = I 
	F = forward 2
	+ = right 90 
	- = left 90 
	[ = push
	] = pop

Rendering after 5 iterations:

<div align="center">
![]($root/images/crystal.jpg)
</div>

## Tiling

Rules:

	I -> F+F+F+F
	F -> FF+F-F+F+FF

Interpretation:

	Start symbol = I 
	F = forward 4 
	+ = right 90 
	- = left 90 
	[ = push
	] = pop

Rendering after 6 iterations:

<div align="center">
![]($root/images/tiles.jpg)
</div>
