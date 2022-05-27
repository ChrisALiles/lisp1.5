package LISP15

// Minimum number of args to be supplied for each function.
var minArgs = map[string]int{
	"add1":       1,
	"atom":       1,
	"car":        1,
	"cdr":        1,
	"cond":       1,
	"cons":       2,
	"define":     0,
	"defun":      2,
	"diff":       2,
	"difference": 2,
	"eq":         2,
	"list":       0,
	"mapcar":     2,
	"null":       1,
	"plus":       2,
	"quit":       0,
	"quote":      1,
	"setq":       2,
	"sub1":       1,
	"times":      2,
	"zerop":      1,
}

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
	//	"mapcar":     {"mapcar", zeroSexpr, applyMapcar, ""},
	"null":  {"null", zeroSexpr, applyNull, ""},
	"plus":  {"plus", zeroSexpr, applyPlus, ""},
	"quit":  {"quit", zeroSexpr, applyQuit, ""},
	"quote": {"quote", zeroSexpr, applyQuote, ""},
	"setq":  {"setq", zeroSexpr, applySetq, ""},
	"sub1":  {"sub1", zeroSexpr, applySub1, ""},
	"times": {"times", zeroSexpr, applyTimes, ""},
	"zerop": {"zerop", zeroSexpr, applyZerop, ""},
}
