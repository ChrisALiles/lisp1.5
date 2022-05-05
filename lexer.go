package LISP15

import (
	"unicode"
	"unicode/utf8"
)

func Lexer(subject string, ichan chan Item) {
	var lex lexer
	lex.input = subject
	lex.items = ichan

	for stateFunc := lexGeneral; stateFunc != nil; {
		stateFunc = stateFunc(&lex)
	}
}

// next returns the next rune in the input.
func (lx *lexer) next() rune {
	if int(lx.pos) >= len(lx.input) {
		lx.width = 0
		return eof
	}
	r, w := utf8.DecodeRuneInString(lx.input[lx.pos:])
	lx.width = Pos(w)
	lx.pos += lx.width
	return r
}

// lexGeneral scans all elements.
func lexGeneral(lx *lexer) stateFn {

	switch nx := lx.next(); {
	case nx == eof:
		return nil
	case nx == '(':
		lx.emit(itemLeftParen)
	case nx == ')':
		lx.emit(itemRightParen)
	case nx == '\'':
		lx.emit(itemQuoteMark)
	case isAlphaBetic(nx):
		lx.backup()
		return lexIdent
	case nx == '.' || nx == '+' || nx == '-' || ('0' <= nx && nx <= '9'):
		lx.backup()
		return lexNumber
	}
	lx.start = lx.pos
	return lexGeneral
}

// lexIdent scans the runes in an identifier/atom.
func lexIdent(lx *lexer) stateFn {

	nx := lx.next()
	if isAlphaNumeric(nx) {
		return lexIdent
	}
	lx.backup()
	atom := lx.input[lx.start:lx.pos]
	code := key[atom]

	if code == 0 {
		code = itemAtom
	}
	lx.emit(code)
	lx.start = lx.pos
	return lexGeneral
}

// lexNumber scans the runes in a number.
func lexNumber(lx *lexer) stateFn {
	nx := lx.next()
	if nx == '.' || nx == '+' || nx == '-' || ('0' <= nx && nx <= '9') {
		return lexNumber
	}
	lx.backup()
	lx.emit(itemNumber)
	lx.start = lx.pos
	return lexGeneral
}

// emit passes an item back to the client.
func (lx *lexer) emit(t int) {
	lx.items <- Item{t, lx.start, lx.input[lx.start:lx.pos]}
	lx.start = lx.pos
}

// backup steps back one rune. Can only be called once
// per call of next.
func (lx *lexer) backup() {
	lx.pos -= lx.width
}

// isAlphaNumeric reports whether r is an alphabetic or digit.
func isAlphaNumeric(r rune) bool {
	return unicode.IsLetter(r) || unicode.IsDigit(r)
}

// isAlphaBetic reports whether r is alphbetic.
func isAlphaBetic(r rune) bool {
	return unicode.IsLetter(r)
}
