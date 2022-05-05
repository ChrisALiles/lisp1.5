package LISP15

// Item type values.
const (
	Prompt   = "LISP>>"
	saveFile = "lispfns"
	nilVal   = "nil"
	trueVal  = "t"
	eof      = -1

	itemError int = iota // error occurred; value is text of error
	itemEOF
	itemAtom
	itemDot        // Dot notation one day
	itemLeftParen  // '('
	itemNumber     // simple number (decimal or integer)
	itemRightParen // ')'
	itemQuoteMark  // the apostrophe, not the "function".
	// Keywords appear after all the rest.
	itemKeyword // used only to delimit the keywords
	// used in item String method
	itemCAR
	itemCOND
	itemCDR
	itemCONS
	itemDEFINE
	itemDEFUN
	itemEQ
	itemQUOTE
	itemLAMBDA
	itemNIL
	itemT
	itemATOM
	itemNULL
	itemAPPEND
	itemMEMBER
	itemQUIT
	itemLIST
	itemPLUS
	itemDIFFERENCE
	itemTIMES
	itemSAVE
	itemLOAD
	itemADD1
	itemSUB1
	itemZEROP

	atomSexpr
	listSexpr
)

var key = map[string]int{
	"car":        itemCAR,
	"cond":       itemCOND,
	"cdr":        itemCDR,
	"cons":       itemCONS,
	"define":     itemDEFINE,
	"defun":      itemDEFUN,
	"eq":         itemEQ,
	"quote":      itemQUOTE,
	"lambda":     itemLAMBDA,
	"nil":        itemNIL,
	"t":          itemT,
	"atom":       itemATOM, // the predicate - not 'an atom'
	"append":     itemAPPEND,
	"member":     itemMEMBER,
	"quit":       itemQUIT,
	"null":       itemNULL,
	"list":       itemLIST,
	"plus":       itemPLUS,
	"difference": itemDIFFERENCE,
	"times":      itemTIMES,
	"save":       itemSAVE,
	"load":       itemLOAD,
	"add1":       itemADD1,
	"sub1":       itemSUB1,
	"zerop":      itemZEROP,
}

var (
	zeroSexpr     sexpr
	zeroSexprList []sexpr
	trueSexpr     = sexpr{
		atomSexpr, true, "T", "T", zeroSexprList,
	}
	nilSexpr = sexpr{
		atomSexpr, true, "NIL", "NIL", zeroSexprList,
	}
)
