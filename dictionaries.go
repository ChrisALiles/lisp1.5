package LISP15

// Table of builtin LISP functions giving the implementing
// functions.
var propList = propertyList{
	"add1":       {"add1", zeroSexpr, applyAdd1, ""},
	"atom":       {"atom", zeroSexpr, applyAtom, ""},
	"car":        {"car", zeroSexpr, applyCar, ""},
	"cdr":        {"cdr", zeroSexpr, applyCdr, ""},
	"cons":       {"cons", zeroSexpr, applyCons, ""},
	"define":     {"define", zeroSexpr, applyDefine, ""},
	"defun":      {"defun", zeroSexpr, applyDefun, ""},
	"diff":       {"diff", zeroSexpr, applyDiff, ""},
	"difference": {"difference", zeroSexpr, applyDiff, ""},
	"eq":         {"eq", zeroSexpr, applyEq, ""},
	"list":       {"list", zeroSexpr, applyList, ""},
	"null":       {"null", zeroSexpr, applyNull, ""},
	"plus":       {"plus", zeroSexpr, applyPlus, ""},
	"quit":       {"quit", zeroSexpr, applyQuit, ""},
	"quote":      {"quote", zeroSexpr, applyQuote, ""},
	"setq":       {"setq", zeroSexpr, applySetq, ""},
	"sub1":       {"sub1", zeroSexpr, applySub1, ""},
	"times":      {"times", zeroSexpr, applyTimes, ""},
	"zerop":      {"zerop", zeroSexpr, applyZerop, ""},
}
