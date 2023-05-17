LISP=sbcl

all: game-engine

game-engine: *.lisp *.asd
	sbcl --non-interactive --eval "(asdf:make :game-engine/executable :debug t)"
