// LISP15 is a partial implementation of the language
// described in the LISP 1.5 Proghrammer's Manual.
// The Lexer was largely adapted from src/text/template/parse/lex.go
// an early version of which was described in Rob Pike's
// "Lexical Scanning in Go" talk.
// SICP was another reference.
package LISP15

import (
	"bytes"
	"encoding/gob"
	"fmt"
	"io"
)

// item represents a token returned from the lexer.
type Item struct {
	typ int    // The type of this item.
	pos Pos    // The starting position, in bytes, of this item in the input string.
	val string // The value of this item.
}

func (i Item) String() string {
	switch {
	case i.typ == itemEOF:
		return "EOF"
	case i.typ == itemError:
		return i.val
	case i.typ > itemKeyword:
		return fmt.Sprintf("<%s>", i.val)
	case len(i.val) > 10:
		return fmt.Sprintf("%.10q...", i.val)
	}
	return fmt.Sprintf("%q", i.val)
}

// Pos represents a byte position in the original input.
type Pos int

func (p Pos) Position() Pos {
	return p
}

// stateFn represents the state of the lexer as a function that returns the next state.
type stateFn func(*lexer) stateFn

// lexer holds the state of the lexer.
type lexer struct {
	input string    // the string being scanned
	pos   Pos       // current position in the input
	start Pos       // start position of this item
	width Pos       // width of last rune read from input
	items chan Item // channel of scanned items
}

// sexpr represents an S-expression.
// The field names are capitalised so gob can be used
// to copy sexprs - see duplicate method below.
type sexpr struct {
	SexprTyp int
	Quoted   bool
	AtomName string
	Val      string
	List     []sexpr
}

// Print representation of an S-expression.
func (sxpr sexpr) String() string {
	if sxpr.SexprTyp == atomSexpr {
		return sxpr.Val
	}
	if len(sxpr.List) == 0 {
		return nilDisplayVal
	}

	str := "("
	for _, subSxpr := range sxpr.List {
		if subSxpr.SexprTyp == atomSexpr {
			str += subSxpr.Val
			str += " "
		} else {
			str += subSxpr.String()
			str += " "
		}
	}
	// Remove the space before the closing paren.
	str = str[:len(str)-1] + ")"
	return str
}

// gob seemed to be the best way of getting a "deep copy"
// of structures with slices in them.
func (sxpr sexpr) duplicate() (sexpr, error) {

	var buf bytes.Buffer
	encoder := gob.NewEncoder(&buf)
	decoder := gob.NewDecoder(&buf)

	err := encoder.Encode(sxpr)
	if err != nil {
		return sxpr, err
	}
	var retSxpr sexpr
	err = decoder.Decode(&retSxpr)
	if err != nil {
		return sxpr, err
	}
	return retSxpr, nil
}

// property is the body of a property list.
// The field names are capitalised so gob can be used
// to back up function definitions.
type property struct {
	Pname string
	Expr  sexpr
	Subr  func(sxpr sexpr) (sexpr, error)
	Apval string
}

func (prop property) duplicate() (property, error) {

	var buf bytes.Buffer
	encoder := gob.NewEncoder(&buf)
	decoder := gob.NewDecoder(&buf)

	err := encoder.Encode(prop)
	if err != nil {
		return prop, err
	}
	var retProp property
	err = decoder.Decode(&retProp)
	if err != nil {
		return prop, err
	}
	return retProp, nil
}

// propertyList holds proprties of built-in functions,
// indexed by function name.
type propertyList map[string]property

// associationList holds variable properties and user
// defined function definitions.
type associationList map[string]property

// environment holds all the association lists.
// [0] is the global environment.
// the environment is created in the init function.
type environMent []associationList

// Add a new environment frame.
func (e *environMent) pushFrame() {

	var newFrame = make(associationList)
	*e = append(*e, newFrame)
}

// Remove the most recent environment frame.
func (e *environMent) popFrame() {

	// note the pointer must be dereference before it
	// can be used as a slice - viz (*e).
	//
	// Don't pop the global environment [0].
	if len(*e) > 1 {
		*e = (*e)[:len(*e)-1]
	}
}

// Find and return the entry for a key.
func (e *environMent) lookUp(key string) (sexpr, bool) {

	// Start at the most recent frame.
	for i := len(*e) - 1; i >= 0; i-- {
		entry, found := (*e)[i][key]
		if found {
			return entry.Expr, true
		}
	}
	return nilSexpr, false
}

// Store the entry for a key.
func (e *environMent) store(key string, val sexpr) {
	entry := (*e)[len(*e)-1][key]
	entry.Pname = key
	entry.Expr = val
	entry.Apval = val.String()
	(*e)[len(*e)-1][key] = entry

}

// Save user defined functions to a file.
func (e *environMent) save(out io.Writer) error {

	var buf bytes.Buffer
	encoder := gob.NewEncoder(&buf)

	for _, entry := range (*e)[0] {
		if entry.Expr.List[0].AtomName != "lambda" {
			continue
		}

		err := encoder.Encode(entry)
		if err != nil {
			return err
		}
		_, err = buf.WriteTo(out)
		if err != nil {
			return err
		}
	}
	return nil
}

// Load user defined function definitions from a file.
func (e *environMent) load(in io.Reader) error {

	var buf bytes.Buffer

	_, err := buf.ReadFrom(in)
	if err != nil {
		return err
	}
	decoder := gob.NewDecoder(&buf)

	var entry property
	for {
		err = decoder.Decode(&entry)
		if err == io.EOF {
			break
		}
		if err != nil {
			return err
		}

		// Duplicate the entry to get rid of any slice
		// pointer entanglements (presuably).
		// (my faith in slices is dwindling.)
		// 23 May 2022 - it's much more likely to be
		// caused by something in gob than in slices.
		dupEntry, err := entry.duplicate()
		if err != nil {
			return err
		}

		environ.store(dupEntry.Expr.AtomName, dupEntry.Expr)
	}
	return nil
}
