package LISP15

// Initialise the environment.
var globalFrame = make(associationList)
var environ = make(environMent, 1, 10)

func init() {
	environ[0] = globalFrame
}
