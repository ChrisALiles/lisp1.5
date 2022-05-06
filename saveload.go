package LISP15

import (
	"fmt"
	"os"
)

// Create file and save user defined function definitions.
func save() error {

	f, err := os.Create("./" + saveFile)
	if err != nil {
		return fmt.Errorf("error opening save file - %v", err)
	}
	err = environ.save(f)
	if err != nil {
		return fmt.Errorf("error saving functions - %v", err)
	}
	return nil
}

// Load user defined function definitions from a file.
func Load() error {

	f, err := os.Open("./" + saveFile)
	if err != nil {
		return nil
	}
	err = environ.load(f)
	if err != nil {
		return fmt.Errorf("error loading functions - %v", err)
	}
	return nil
}
