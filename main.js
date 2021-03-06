(function(scope){
'use strict';

function F(arity, fun, wrapper) {
  wrapper.a = arity;
  wrapper.f = fun;
  return wrapper;
}

function F2(fun) {
  return F(2, fun, function(a) { return function(b) { return fun(a,b); }; })
}
function F3(fun) {
  return F(3, fun, function(a) {
    return function(b) { return function(c) { return fun(a, b, c); }; };
  });
}
function F4(fun) {
  return F(4, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return fun(a, b, c, d); }; }; };
  });
}
function F5(fun) {
  return F(5, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return fun(a, b, c, d, e); }; }; }; };
  });
}
function F6(fun) {
  return F(6, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return fun(a, b, c, d, e, f); }; }; }; }; };
  });
}
function F7(fun) {
  return F(7, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return fun(a, b, c, d, e, f, g); }; }; }; }; }; };
  });
}
function F8(fun) {
  return F(8, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) {
    return fun(a, b, c, d, e, f, g, h); }; }; }; }; }; }; };
  });
}
function F9(fun) {
  return F(9, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) { return function(i) {
    return fun(a, b, c, d, e, f, g, h, i); }; }; }; }; }; }; }; };
  });
}

function A2(fun, a, b) {
  return fun.a === 2 ? fun.f(a, b) : fun(a)(b);
}
function A3(fun, a, b, c) {
  return fun.a === 3 ? fun.f(a, b, c) : fun(a)(b)(c);
}
function A4(fun, a, b, c, d) {
  return fun.a === 4 ? fun.f(a, b, c, d) : fun(a)(b)(c)(d);
}
function A5(fun, a, b, c, d, e) {
  return fun.a === 5 ? fun.f(a, b, c, d, e) : fun(a)(b)(c)(d)(e);
}
function A6(fun, a, b, c, d, e, f) {
  return fun.a === 6 ? fun.f(a, b, c, d, e, f) : fun(a)(b)(c)(d)(e)(f);
}
function A7(fun, a, b, c, d, e, f, g) {
  return fun.a === 7 ? fun.f(a, b, c, d, e, f, g) : fun(a)(b)(c)(d)(e)(f)(g);
}
function A8(fun, a, b, c, d, e, f, g, h) {
  return fun.a === 8 ? fun.f(a, b, c, d, e, f, g, h) : fun(a)(b)(c)(d)(e)(f)(g)(h);
}
function A9(fun, a, b, c, d, e, f, g, h, i) {
  return fun.a === 9 ? fun.f(a, b, c, d, e, f, g, h, i) : fun(a)(b)(c)(d)(e)(f)(g)(h)(i);
}




// EQUALITY

function _Utils_eq(x, y)
{
	for (
		var pair, stack = [], isEqual = _Utils_eqHelp(x, y, 0, stack);
		isEqual && (pair = stack.pop());
		isEqual = _Utils_eqHelp(pair.a, pair.b, 0, stack)
		)
	{}

	return isEqual;
}

function _Utils_eqHelp(x, y, depth, stack)
{
	if (x === y)
	{
		return true;
	}

	if (typeof x !== 'object' || x === null || y === null)
	{
		typeof x === 'function' && _Debug_crash(5);
		return false;
	}

	if (depth > 100)
	{
		stack.push(_Utils_Tuple2(x,y));
		return true;
	}

	/**_UNUSED/
	if (x.$ === 'Set_elm_builtin')
	{
		x = $elm$core$Set$toList(x);
		y = $elm$core$Set$toList(y);
	}
	if (x.$ === 'RBNode_elm_builtin' || x.$ === 'RBEmpty_elm_builtin')
	{
		x = $elm$core$Dict$toList(x);
		y = $elm$core$Dict$toList(y);
	}
	//*/

	/**/
	if (x.$ < 0)
	{
		x = $elm$core$Dict$toList(x);
		y = $elm$core$Dict$toList(y);
	}
	//*/

	for (var key in x)
	{
		if (!_Utils_eqHelp(x[key], y[key], depth + 1, stack))
		{
			return false;
		}
	}
	return true;
}

var _Utils_equal = F2(_Utils_eq);
var _Utils_notEqual = F2(function(a, b) { return !_Utils_eq(a,b); });



// COMPARISONS

// Code in Generate/JavaScript.hs, Basics.js, and List.js depends on
// the particular integer values assigned to LT, EQ, and GT.

function _Utils_cmp(x, y, ord)
{
	if (typeof x !== 'object')
	{
		return x === y ? /*EQ*/ 0 : x < y ? /*LT*/ -1 : /*GT*/ 1;
	}

	/**_UNUSED/
	if (x instanceof String)
	{
		var a = x.valueOf();
		var b = y.valueOf();
		return a === b ? 0 : a < b ? -1 : 1;
	}
	//*/

	/**/
	if (typeof x.$ === 'undefined')
	//*/
	/**_UNUSED/
	if (x.$[0] === '#')
	//*/
	{
		return (ord = _Utils_cmp(x.a, y.a))
			? ord
			: (ord = _Utils_cmp(x.b, y.b))
				? ord
				: _Utils_cmp(x.c, y.c);
	}

	// traverse conses until end of a list or a mismatch
	for (; x.b && y.b && !(ord = _Utils_cmp(x.a, y.a)); x = x.b, y = y.b) {} // WHILE_CONSES
	return ord || (x.b ? /*GT*/ 1 : y.b ? /*LT*/ -1 : /*EQ*/ 0);
}

var _Utils_lt = F2(function(a, b) { return _Utils_cmp(a, b) < 0; });
var _Utils_le = F2(function(a, b) { return _Utils_cmp(a, b) < 1; });
var _Utils_gt = F2(function(a, b) { return _Utils_cmp(a, b) > 0; });
var _Utils_ge = F2(function(a, b) { return _Utils_cmp(a, b) >= 0; });

var _Utils_compare = F2(function(x, y)
{
	var n = _Utils_cmp(x, y);
	return n < 0 ? $elm$core$Basics$LT : n ? $elm$core$Basics$GT : $elm$core$Basics$EQ;
});


// COMMON VALUES

var _Utils_Tuple0 = 0;
var _Utils_Tuple0_UNUSED = { $: '#0' };

function _Utils_Tuple2(a, b) { return { a: a, b: b }; }
function _Utils_Tuple2_UNUSED(a, b) { return { $: '#2', a: a, b: b }; }

function _Utils_Tuple3(a, b, c) { return { a: a, b: b, c: c }; }
function _Utils_Tuple3_UNUSED(a, b, c) { return { $: '#3', a: a, b: b, c: c }; }

function _Utils_chr(c) { return c; }
function _Utils_chr_UNUSED(c) { return new String(c); }


// RECORDS

function _Utils_update(oldRecord, updatedFields)
{
	var newRecord = {};

	for (var key in oldRecord)
	{
		newRecord[key] = oldRecord[key];
	}

	for (var key in updatedFields)
	{
		newRecord[key] = updatedFields[key];
	}

	return newRecord;
}


// APPEND

var _Utils_append = F2(_Utils_ap);

function _Utils_ap(xs, ys)
{
	// append Strings
	if (typeof xs === 'string')
	{
		return xs + ys;
	}

	// append Lists
	if (!xs.b)
	{
		return ys;
	}
	var root = _List_Cons(xs.a, ys);
	xs = xs.b
	for (var curr = root; xs.b; xs = xs.b) // WHILE_CONS
	{
		curr = curr.b = _List_Cons(xs.a, ys);
	}
	return root;
}



var _List_Nil = { $: 0 };
var _List_Nil_UNUSED = { $: '[]' };

function _List_Cons(hd, tl) { return { $: 1, a: hd, b: tl }; }
function _List_Cons_UNUSED(hd, tl) { return { $: '::', a: hd, b: tl }; }


var _List_cons = F2(_List_Cons);

function _List_fromArray(arr)
{
	var out = _List_Nil;
	for (var i = arr.length; i--; )
	{
		out = _List_Cons(arr[i], out);
	}
	return out;
}

function _List_toArray(xs)
{
	for (var out = []; xs.b; xs = xs.b) // WHILE_CONS
	{
		out.push(xs.a);
	}
	return out;
}

var _List_map2 = F3(function(f, xs, ys)
{
	for (var arr = []; xs.b && ys.b; xs = xs.b, ys = ys.b) // WHILE_CONSES
	{
		arr.push(A2(f, xs.a, ys.a));
	}
	return _List_fromArray(arr);
});

var _List_map3 = F4(function(f, xs, ys, zs)
{
	for (var arr = []; xs.b && ys.b && zs.b; xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A3(f, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map4 = F5(function(f, ws, xs, ys, zs)
{
	for (var arr = []; ws.b && xs.b && ys.b && zs.b; ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A4(f, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map5 = F6(function(f, vs, ws, xs, ys, zs)
{
	for (var arr = []; vs.b && ws.b && xs.b && ys.b && zs.b; vs = vs.b, ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A5(f, vs.a, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_sortBy = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		return _Utils_cmp(f(a), f(b));
	}));
});

var _List_sortWith = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		var ord = A2(f, a, b);
		return ord === $elm$core$Basics$EQ ? 0 : ord === $elm$core$Basics$LT ? -1 : 1;
	}));
});



var _JsArray_empty = [];

function _JsArray_singleton(value)
{
    return [value];
}

function _JsArray_length(array)
{
    return array.length;
}

var _JsArray_initialize = F3(function(size, offset, func)
{
    var result = new Array(size);

    for (var i = 0; i < size; i++)
    {
        result[i] = func(offset + i);
    }

    return result;
});

var _JsArray_initializeFromList = F2(function (max, ls)
{
    var result = new Array(max);

    for (var i = 0; i < max && ls.b; i++)
    {
        result[i] = ls.a;
        ls = ls.b;
    }

    result.length = i;
    return _Utils_Tuple2(result, ls);
});

var _JsArray_unsafeGet = F2(function(index, array)
{
    return array[index];
});

var _JsArray_unsafeSet = F3(function(index, value, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[index] = value;
    return result;
});

var _JsArray_push = F2(function(value, array)
{
    var length = array.length;
    var result = new Array(length + 1);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[length] = value;
    return result;
});

var _JsArray_foldl = F3(function(func, acc, array)
{
    var length = array.length;

    for (var i = 0; i < length; i++)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_foldr = F3(function(func, acc, array)
{
    for (var i = array.length - 1; i >= 0; i--)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_map = F2(function(func, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = func(array[i]);
    }

    return result;
});

var _JsArray_indexedMap = F3(function(func, offset, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = A2(func, offset + i, array[i]);
    }

    return result;
});

var _JsArray_slice = F3(function(from, to, array)
{
    return array.slice(from, to);
});

var _JsArray_appendN = F3(function(n, dest, source)
{
    var destLen = dest.length;
    var itemsToCopy = n - destLen;

    if (itemsToCopy > source.length)
    {
        itemsToCopy = source.length;
    }

    var size = destLen + itemsToCopy;
    var result = new Array(size);

    for (var i = 0; i < destLen; i++)
    {
        result[i] = dest[i];
    }

    for (var i = 0; i < itemsToCopy; i++)
    {
        result[i + destLen] = source[i];
    }

    return result;
});



// LOG

var _Debug_log = F2(function(tag, value)
{
	return value;
});

var _Debug_log_UNUSED = F2(function(tag, value)
{
	console.log(tag + ': ' + _Debug_toString(value));
	return value;
});


// TODOS

function _Debug_todo(moduleName, region)
{
	return function(message) {
		_Debug_crash(8, moduleName, region, message);
	};
}

function _Debug_todoCase(moduleName, region, value)
{
	return function(message) {
		_Debug_crash(9, moduleName, region, value, message);
	};
}


// TO STRING

function _Debug_toString(value)
{
	return '<internals>';
}

function _Debug_toString_UNUSED(value)
{
	return _Debug_toAnsiString(false, value);
}

function _Debug_toAnsiString(ansi, value)
{
	if (typeof value === 'function')
	{
		return _Debug_internalColor(ansi, '<function>');
	}

	if (typeof value === 'boolean')
	{
		return _Debug_ctorColor(ansi, value ? 'True' : 'False');
	}

	if (typeof value === 'number')
	{
		return _Debug_numberColor(ansi, value + '');
	}

	if (value instanceof String)
	{
		return _Debug_charColor(ansi, "'" + _Debug_addSlashes(value, true) + "'");
	}

	if (typeof value === 'string')
	{
		return _Debug_stringColor(ansi, '"' + _Debug_addSlashes(value, false) + '"');
	}

	if (typeof value === 'object' && '$' in value)
	{
		var tag = value.$;

		if (typeof tag === 'number')
		{
			return _Debug_internalColor(ansi, '<internals>');
		}

		if (tag[0] === '#')
		{
			var output = [];
			for (var k in value)
			{
				if (k === '$') continue;
				output.push(_Debug_toAnsiString(ansi, value[k]));
			}
			return '(' + output.join(',') + ')';
		}

		if (tag === 'Set_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Set')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Set$toList(value));
		}

		if (tag === 'RBNode_elm_builtin' || tag === 'RBEmpty_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Dict')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Dict$toList(value));
		}

		if (tag === 'Array_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Array')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Array$toList(value));
		}

		if (tag === '::' || tag === '[]')
		{
			var output = '[';

			value.b && (output += _Debug_toAnsiString(ansi, value.a), value = value.b)

			for (; value.b; value = value.b) // WHILE_CONS
			{
				output += ',' + _Debug_toAnsiString(ansi, value.a);
			}
			return output + ']';
		}

		var output = '';
		for (var i in value)
		{
			if (i === '$') continue;
			var str = _Debug_toAnsiString(ansi, value[i]);
			var c0 = str[0];
			var parenless = c0 === '{' || c0 === '(' || c0 === '[' || c0 === '<' || c0 === '"' || str.indexOf(' ') < 0;
			output += ' ' + (parenless ? str : '(' + str + ')');
		}
		return _Debug_ctorColor(ansi, tag) + output;
	}

	if (typeof DataView === 'function' && value instanceof DataView)
	{
		return _Debug_stringColor(ansi, '<' + value.byteLength + ' bytes>');
	}

	if (typeof File !== 'undefined' && value instanceof File)
	{
		return _Debug_internalColor(ansi, '<' + value.name + '>');
	}

	if (typeof value === 'object')
	{
		var output = [];
		for (var key in value)
		{
			var field = key[0] === '_' ? key.slice(1) : key;
			output.push(_Debug_fadeColor(ansi, field) + ' = ' + _Debug_toAnsiString(ansi, value[key]));
		}
		if (output.length === 0)
		{
			return '{}';
		}
		return '{ ' + output.join(', ') + ' }';
	}

	return _Debug_internalColor(ansi, '<internals>');
}

function _Debug_addSlashes(str, isChar)
{
	var s = str
		.replace(/\\/g, '\\\\')
		.replace(/\n/g, '\\n')
		.replace(/\t/g, '\\t')
		.replace(/\r/g, '\\r')
		.replace(/\v/g, '\\v')
		.replace(/\0/g, '\\0');

	if (isChar)
	{
		return s.replace(/\'/g, '\\\'');
	}
	else
	{
		return s.replace(/\"/g, '\\"');
	}
}

function _Debug_ctorColor(ansi, string)
{
	return ansi ? '\x1b[96m' + string + '\x1b[0m' : string;
}

function _Debug_numberColor(ansi, string)
{
	return ansi ? '\x1b[95m' + string + '\x1b[0m' : string;
}

function _Debug_stringColor(ansi, string)
{
	return ansi ? '\x1b[93m' + string + '\x1b[0m' : string;
}

function _Debug_charColor(ansi, string)
{
	return ansi ? '\x1b[92m' + string + '\x1b[0m' : string;
}

function _Debug_fadeColor(ansi, string)
{
	return ansi ? '\x1b[37m' + string + '\x1b[0m' : string;
}

function _Debug_internalColor(ansi, string)
{
	return ansi ? '\x1b[36m' + string + '\x1b[0m' : string;
}

function _Debug_toHexDigit(n)
{
	return String.fromCharCode(n < 10 ? 48 + n : 55 + n);
}


// CRASH


function _Debug_crash(identifier)
{
	throw new Error('https://github.com/elm/core/blob/1.0.0/hints/' + identifier + '.md');
}


function _Debug_crash_UNUSED(identifier, fact1, fact2, fact3, fact4)
{
	switch(identifier)
	{
		case 0:
			throw new Error('What node should I take over? In JavaScript I need something like:\n\n    Elm.Main.init({\n        node: document.getElementById("elm-node")\n    })\n\nYou need to do this with any Browser.sandbox or Browser.element program.');

		case 1:
			throw new Error('Browser.application programs cannot handle URLs like this:\n\n    ' + document.location.href + '\n\nWhat is the root? The root of your file system? Try looking at this program with `elm reactor` or some other server.');

		case 2:
			var jsonErrorString = fact1;
			throw new Error('Problem with the flags given to your Elm program on initialization.\n\n' + jsonErrorString);

		case 3:
			var portName = fact1;
			throw new Error('There can only be one port named `' + portName + '`, but your program has multiple.');

		case 4:
			var portName = fact1;
			var problem = fact2;
			throw new Error('Trying to send an unexpected type of value through port `' + portName + '`:\n' + problem);

		case 5:
			throw new Error('Trying to use `(==)` on functions.\nThere is no way to know if functions are "the same" in the Elm sense.\nRead more about this at https://package.elm-lang.org/packages/elm/core/latest/Basics#== which describes why it is this way and what the better version will look like.');

		case 6:
			var moduleName = fact1;
			throw new Error('Your page is loading multiple Elm scripts with a module named ' + moduleName + '. Maybe a duplicate script is getting loaded accidentally? If not, rename one of them so I know which is which!');

		case 8:
			var moduleName = fact1;
			var region = fact2;
			var message = fact3;
			throw new Error('TODO in module `' + moduleName + '` ' + _Debug_regionToString(region) + '\n\n' + message);

		case 9:
			var moduleName = fact1;
			var region = fact2;
			var value = fact3;
			var message = fact4;
			throw new Error(
				'TODO in module `' + moduleName + '` from the `case` expression '
				+ _Debug_regionToString(region) + '\n\nIt received the following value:\n\n    '
				+ _Debug_toString(value).replace('\n', '\n    ')
				+ '\n\nBut the branch that handles it says:\n\n    ' + message.replace('\n', '\n    ')
			);

		case 10:
			throw new Error('Bug in https://github.com/elm/virtual-dom/issues');

		case 11:
			throw new Error('Cannot perform mod 0. Division by zero error.');
	}
}

function _Debug_regionToString(region)
{
	if (region.aQ.af === region.a$.af)
	{
		return 'on line ' + region.aQ.af;
	}
	return 'on lines ' + region.aQ.af + ' through ' + region.a$.af;
}



// MATH

var _Basics_add = F2(function(a, b) { return a + b; });
var _Basics_sub = F2(function(a, b) { return a - b; });
var _Basics_mul = F2(function(a, b) { return a * b; });
var _Basics_fdiv = F2(function(a, b) { return a / b; });
var _Basics_idiv = F2(function(a, b) { return (a / b) | 0; });
var _Basics_pow = F2(Math.pow);

var _Basics_remainderBy = F2(function(b, a) { return a % b; });

// https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/divmodnote-letter.pdf
var _Basics_modBy = F2(function(modulus, x)
{
	var answer = x % modulus;
	return modulus === 0
		? _Debug_crash(11)
		:
	((answer > 0 && modulus < 0) || (answer < 0 && modulus > 0))
		? answer + modulus
		: answer;
});


// TRIGONOMETRY

var _Basics_pi = Math.PI;
var _Basics_e = Math.E;
var _Basics_cos = Math.cos;
var _Basics_sin = Math.sin;
var _Basics_tan = Math.tan;
var _Basics_acos = Math.acos;
var _Basics_asin = Math.asin;
var _Basics_atan = Math.atan;
var _Basics_atan2 = F2(Math.atan2);


// MORE MATH

function _Basics_toFloat(x) { return x; }
function _Basics_truncate(n) { return n | 0; }
function _Basics_isInfinite(n) { return n === Infinity || n === -Infinity; }

var _Basics_ceiling = Math.ceil;
var _Basics_floor = Math.floor;
var _Basics_round = Math.round;
var _Basics_sqrt = Math.sqrt;
var _Basics_log = Math.log;
var _Basics_isNaN = isNaN;


// BOOLEANS

function _Basics_not(bool) { return !bool; }
var _Basics_and = F2(function(a, b) { return a && b; });
var _Basics_or  = F2(function(a, b) { return a || b; });
var _Basics_xor = F2(function(a, b) { return a !== b; });



var _String_cons = F2(function(chr, str)
{
	return chr + str;
});

function _String_uncons(string)
{
	var word = string.charCodeAt(0);
	return !isNaN(word)
		? $elm$core$Maybe$Just(
			0xD800 <= word && word <= 0xDBFF
				? _Utils_Tuple2(_Utils_chr(string[0] + string[1]), string.slice(2))
				: _Utils_Tuple2(_Utils_chr(string[0]), string.slice(1))
		)
		: $elm$core$Maybe$Nothing;
}

var _String_append = F2(function(a, b)
{
	return a + b;
});

function _String_length(str)
{
	return str.length;
}

var _String_map = F2(function(func, string)
{
	var len = string.length;
	var array = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = string.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			array[i] = func(_Utils_chr(string[i] + string[i+1]));
			i += 2;
			continue;
		}
		array[i] = func(_Utils_chr(string[i]));
		i++;
	}
	return array.join('');
});

var _String_filter = F2(function(isGood, str)
{
	var arr = [];
	var len = str.length;
	var i = 0;
	while (i < len)
	{
		var char = str[i];
		var word = str.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += str[i];
			i++;
		}

		if (isGood(_Utils_chr(char)))
		{
			arr.push(char);
		}
	}
	return arr.join('');
});

function _String_reverse(str)
{
	var len = str.length;
	var arr = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = str.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			arr[len - i] = str[i + 1];
			i++;
			arr[len - i] = str[i - 1];
			i++;
		}
		else
		{
			arr[len - i] = str[i];
			i++;
		}
	}
	return arr.join('');
}

var _String_foldl = F3(function(func, state, string)
{
	var len = string.length;
	var i = 0;
	while (i < len)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += string[i];
			i++;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_foldr = F3(function(func, state, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_split = F2(function(sep, str)
{
	return str.split(sep);
});

var _String_join = F2(function(sep, strs)
{
	return strs.join(sep);
});

var _String_slice = F3(function(start, end, str) {
	return str.slice(start, end);
});

function _String_trim(str)
{
	return str.trim();
}

function _String_trimLeft(str)
{
	return str.replace(/^\s+/, '');
}

function _String_trimRight(str)
{
	return str.replace(/\s+$/, '');
}

function _String_words(str)
{
	return _List_fromArray(str.trim().split(/\s+/g));
}

function _String_lines(str)
{
	return _List_fromArray(str.split(/\r\n|\r|\n/g));
}

function _String_toUpper(str)
{
	return str.toUpperCase();
}

function _String_toLower(str)
{
	return str.toLowerCase();
}

var _String_any = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (isGood(_Utils_chr(char)))
		{
			return true;
		}
	}
	return false;
});

var _String_all = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (!isGood(_Utils_chr(char)))
		{
			return false;
		}
	}
	return true;
});

var _String_contains = F2(function(sub, str)
{
	return str.indexOf(sub) > -1;
});

var _String_startsWith = F2(function(sub, str)
{
	return str.indexOf(sub) === 0;
});

var _String_endsWith = F2(function(sub, str)
{
	return str.length >= sub.length &&
		str.lastIndexOf(sub) === str.length - sub.length;
});

var _String_indexes = F2(function(sub, str)
{
	var subLen = sub.length;

	if (subLen < 1)
	{
		return _List_Nil;
	}

	var i = 0;
	var is = [];

	while ((i = str.indexOf(sub, i)) > -1)
	{
		is.push(i);
		i = i + subLen;
	}

	return _List_fromArray(is);
});


// TO STRING

function _String_fromNumber(number)
{
	return number + '';
}


// INT CONVERSIONS

function _String_toInt(str)
{
	var total = 0;
	var code0 = str.charCodeAt(0);
	var start = code0 == 0x2B /* + */ || code0 == 0x2D /* - */ ? 1 : 0;

	for (var i = start; i < str.length; ++i)
	{
		var code = str.charCodeAt(i);
		if (code < 0x30 || 0x39 < code)
		{
			return $elm$core$Maybe$Nothing;
		}
		total = 10 * total + code - 0x30;
	}

	return i == start
		? $elm$core$Maybe$Nothing
		: $elm$core$Maybe$Just(code0 == 0x2D ? -total : total);
}


// FLOAT CONVERSIONS

function _String_toFloat(s)
{
	// check if it is a hex, octal, or binary number
	if (s.length === 0 || /[\sxbo]/.test(s))
	{
		return $elm$core$Maybe$Nothing;
	}
	var n = +s;
	// faster isNaN check
	return n === n ? $elm$core$Maybe$Just(n) : $elm$core$Maybe$Nothing;
}

function _String_fromList(chars)
{
	return _List_toArray(chars).join('');
}




function _Char_toCode(char)
{
	var code = char.charCodeAt(0);
	if (0xD800 <= code && code <= 0xDBFF)
	{
		return (code - 0xD800) * 0x400 + char.charCodeAt(1) - 0xDC00 + 0x10000
	}
	return code;
}

function _Char_fromCode(code)
{
	return _Utils_chr(
		(code < 0 || 0x10FFFF < code)
			? '\uFFFD'
			:
		(code <= 0xFFFF)
			? String.fromCharCode(code)
			:
		(code -= 0x10000,
			String.fromCharCode(Math.floor(code / 0x400) + 0xD800, code % 0x400 + 0xDC00)
		)
	);
}

function _Char_toUpper(char)
{
	return _Utils_chr(char.toUpperCase());
}

function _Char_toLower(char)
{
	return _Utils_chr(char.toLowerCase());
}

function _Char_toLocaleUpper(char)
{
	return _Utils_chr(char.toLocaleUpperCase());
}

function _Char_toLocaleLower(char)
{
	return _Utils_chr(char.toLocaleLowerCase());
}



/**_UNUSED/
function _Json_errorToString(error)
{
	return $elm$json$Json$Decode$errorToString(error);
}
//*/


// CORE DECODERS

function _Json_succeed(msg)
{
	return {
		$: 0,
		a: msg
	};
}

function _Json_fail(msg)
{
	return {
		$: 1,
		a: msg
	};
}

function _Json_decodePrim(decoder)
{
	return { $: 2, b: decoder };
}

var _Json_decodeInt = _Json_decodePrim(function(value) {
	return (typeof value !== 'number')
		? _Json_expecting('an INT', value)
		:
	(-2147483647 < value && value < 2147483647 && (value | 0) === value)
		? $elm$core$Result$Ok(value)
		:
	(isFinite(value) && !(value % 1))
		? $elm$core$Result$Ok(value)
		: _Json_expecting('an INT', value);
});

var _Json_decodeBool = _Json_decodePrim(function(value) {
	return (typeof value === 'boolean')
		? $elm$core$Result$Ok(value)
		: _Json_expecting('a BOOL', value);
});

var _Json_decodeFloat = _Json_decodePrim(function(value) {
	return (typeof value === 'number')
		? $elm$core$Result$Ok(value)
		: _Json_expecting('a FLOAT', value);
});

var _Json_decodeValue = _Json_decodePrim(function(value) {
	return $elm$core$Result$Ok(_Json_wrap(value));
});

var _Json_decodeString = _Json_decodePrim(function(value) {
	return (typeof value === 'string')
		? $elm$core$Result$Ok(value)
		: (value instanceof String)
			? $elm$core$Result$Ok(value + '')
			: _Json_expecting('a STRING', value);
});

function _Json_decodeList(decoder) { return { $: 3, b: decoder }; }
function _Json_decodeArray(decoder) { return { $: 4, b: decoder }; }

function _Json_decodeNull(value) { return { $: 5, c: value }; }

var _Json_decodeField = F2(function(field, decoder)
{
	return {
		$: 6,
		d: field,
		b: decoder
	};
});

var _Json_decodeIndex = F2(function(index, decoder)
{
	return {
		$: 7,
		e: index,
		b: decoder
	};
});

function _Json_decodeKeyValuePairs(decoder)
{
	return {
		$: 8,
		b: decoder
	};
}

function _Json_mapMany(f, decoders)
{
	return {
		$: 9,
		f: f,
		g: decoders
	};
}

var _Json_andThen = F2(function(callback, decoder)
{
	return {
		$: 10,
		b: decoder,
		h: callback
	};
});

function _Json_oneOf(decoders)
{
	return {
		$: 11,
		g: decoders
	};
}


// DECODING OBJECTS

var _Json_map1 = F2(function(f, d1)
{
	return _Json_mapMany(f, [d1]);
});

var _Json_map2 = F3(function(f, d1, d2)
{
	return _Json_mapMany(f, [d1, d2]);
});

var _Json_map3 = F4(function(f, d1, d2, d3)
{
	return _Json_mapMany(f, [d1, d2, d3]);
});

var _Json_map4 = F5(function(f, d1, d2, d3, d4)
{
	return _Json_mapMany(f, [d1, d2, d3, d4]);
});

var _Json_map5 = F6(function(f, d1, d2, d3, d4, d5)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5]);
});

var _Json_map6 = F7(function(f, d1, d2, d3, d4, d5, d6)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6]);
});

var _Json_map7 = F8(function(f, d1, d2, d3, d4, d5, d6, d7)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7]);
});

var _Json_map8 = F9(function(f, d1, d2, d3, d4, d5, d6, d7, d8)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7, d8]);
});


// DECODE

var _Json_runOnString = F2(function(decoder, string)
{
	try
	{
		var value = JSON.parse(string);
		return _Json_runHelp(decoder, value);
	}
	catch (e)
	{
		return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, 'This is not valid JSON! ' + e.message, _Json_wrap(string)));
	}
});

var _Json_run = F2(function(decoder, value)
{
	return _Json_runHelp(decoder, _Json_unwrap(value));
});

function _Json_runHelp(decoder, value)
{
	switch (decoder.$)
	{
		case 2:
			return decoder.b(value);

		case 5:
			return (value === null)
				? $elm$core$Result$Ok(decoder.c)
				: _Json_expecting('null', value);

		case 3:
			if (!_Json_isArray(value))
			{
				return _Json_expecting('a LIST', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _List_fromArray);

		case 4:
			if (!_Json_isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _Json_toElmArray);

		case 6:
			var field = decoder.d;
			if (typeof value !== 'object' || value === null || !(field in value))
			{
				return _Json_expecting('an OBJECT with a field named `' + field + '`', value);
			}
			var result = _Json_runHelp(decoder.b, value[field]);
			return ($elm$core$Result$isOk(result)) ? result : $elm$core$Result$Err(A2($elm$json$Json$Decode$Field, field, result.a));

		case 7:
			var index = decoder.e;
			if (!_Json_isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			if (index >= value.length)
			{
				return _Json_expecting('a LONGER array. Need index ' + index + ' but only see ' + value.length + ' entries', value);
			}
			var result = _Json_runHelp(decoder.b, value[index]);
			return ($elm$core$Result$isOk(result)) ? result : $elm$core$Result$Err(A2($elm$json$Json$Decode$Index, index, result.a));

		case 8:
			if (typeof value !== 'object' || value === null || _Json_isArray(value))
			{
				return _Json_expecting('an OBJECT', value);
			}

			var keyValuePairs = _List_Nil;
			// TODO test perf of Object.keys and switch when support is good enough
			for (var key in value)
			{
				if (value.hasOwnProperty(key))
				{
					var result = _Json_runHelp(decoder.b, value[key]);
					if (!$elm$core$Result$isOk(result))
					{
						return $elm$core$Result$Err(A2($elm$json$Json$Decode$Field, key, result.a));
					}
					keyValuePairs = _List_Cons(_Utils_Tuple2(key, result.a), keyValuePairs);
				}
			}
			return $elm$core$Result$Ok($elm$core$List$reverse(keyValuePairs));

		case 9:
			var answer = decoder.f;
			var decoders = decoder.g;
			for (var i = 0; i < decoders.length; i++)
			{
				var result = _Json_runHelp(decoders[i], value);
				if (!$elm$core$Result$isOk(result))
				{
					return result;
				}
				answer = answer(result.a);
			}
			return $elm$core$Result$Ok(answer);

		case 10:
			var result = _Json_runHelp(decoder.b, value);
			return (!$elm$core$Result$isOk(result))
				? result
				: _Json_runHelp(decoder.h(result.a), value);

		case 11:
			var errors = _List_Nil;
			for (var temp = decoder.g; temp.b; temp = temp.b) // WHILE_CONS
			{
				var result = _Json_runHelp(temp.a, value);
				if ($elm$core$Result$isOk(result))
				{
					return result;
				}
				errors = _List_Cons(result.a, errors);
			}
			return $elm$core$Result$Err($elm$json$Json$Decode$OneOf($elm$core$List$reverse(errors)));

		case 1:
			return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, decoder.a, _Json_wrap(value)));

		case 0:
			return $elm$core$Result$Ok(decoder.a);
	}
}

function _Json_runArrayDecoder(decoder, value, toElmValue)
{
	var len = value.length;
	var array = new Array(len);
	for (var i = 0; i < len; i++)
	{
		var result = _Json_runHelp(decoder, value[i]);
		if (!$elm$core$Result$isOk(result))
		{
			return $elm$core$Result$Err(A2($elm$json$Json$Decode$Index, i, result.a));
		}
		array[i] = result.a;
	}
	return $elm$core$Result$Ok(toElmValue(array));
}

function _Json_isArray(value)
{
	return Array.isArray(value) || (typeof FileList !== 'undefined' && value instanceof FileList);
}

function _Json_toElmArray(array)
{
	return A2($elm$core$Array$initialize, array.length, function(i) { return array[i]; });
}

function _Json_expecting(type, value)
{
	return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, 'Expecting ' + type, _Json_wrap(value)));
}


// EQUALITY

function _Json_equality(x, y)
{
	if (x === y)
	{
		return true;
	}

	if (x.$ !== y.$)
	{
		return false;
	}

	switch (x.$)
	{
		case 0:
		case 1:
			return x.a === y.a;

		case 2:
			return x.b === y.b;

		case 5:
			return x.c === y.c;

		case 3:
		case 4:
		case 8:
			return _Json_equality(x.b, y.b);

		case 6:
			return x.d === y.d && _Json_equality(x.b, y.b);

		case 7:
			return x.e === y.e && _Json_equality(x.b, y.b);

		case 9:
			return x.f === y.f && _Json_listEquality(x.g, y.g);

		case 10:
			return x.h === y.h && _Json_equality(x.b, y.b);

		case 11:
			return _Json_listEquality(x.g, y.g);
	}
}

function _Json_listEquality(aDecoders, bDecoders)
{
	var len = aDecoders.length;
	if (len !== bDecoders.length)
	{
		return false;
	}
	for (var i = 0; i < len; i++)
	{
		if (!_Json_equality(aDecoders[i], bDecoders[i]))
		{
			return false;
		}
	}
	return true;
}


// ENCODE

var _Json_encode = F2(function(indentLevel, value)
{
	return JSON.stringify(_Json_unwrap(value), null, indentLevel) + '';
});

function _Json_wrap_UNUSED(value) { return { $: 0, a: value }; }
function _Json_unwrap_UNUSED(value) { return value.a; }

function _Json_wrap(value) { return value; }
function _Json_unwrap(value) { return value; }

function _Json_emptyArray() { return []; }
function _Json_emptyObject() { return {}; }

var _Json_addField = F3(function(key, value, object)
{
	object[key] = _Json_unwrap(value);
	return object;
});

function _Json_addEntry(func)
{
	return F2(function(entry, array)
	{
		array.push(_Json_unwrap(func(entry)));
		return array;
	});
}

var _Json_encodeNull = _Json_wrap(null);



// TASKS

function _Scheduler_succeed(value)
{
	return {
		$: 0,
		a: value
	};
}

function _Scheduler_fail(error)
{
	return {
		$: 1,
		a: error
	};
}

function _Scheduler_binding(callback)
{
	return {
		$: 2,
		b: callback,
		c: null
	};
}

var _Scheduler_andThen = F2(function(callback, task)
{
	return {
		$: 3,
		b: callback,
		d: task
	};
});

var _Scheduler_onError = F2(function(callback, task)
{
	return {
		$: 4,
		b: callback,
		d: task
	};
});

function _Scheduler_receive(callback)
{
	return {
		$: 5,
		b: callback
	};
}


// PROCESSES

var _Scheduler_guid = 0;

function _Scheduler_rawSpawn(task)
{
	var proc = {
		$: 0,
		e: _Scheduler_guid++,
		f: task,
		g: null,
		h: []
	};

	_Scheduler_enqueue(proc);

	return proc;
}

function _Scheduler_spawn(task)
{
	return _Scheduler_binding(function(callback) {
		callback(_Scheduler_succeed(_Scheduler_rawSpawn(task)));
	});
}

function _Scheduler_rawSend(proc, msg)
{
	proc.h.push(msg);
	_Scheduler_enqueue(proc);
}

var _Scheduler_send = F2(function(proc, msg)
{
	return _Scheduler_binding(function(callback) {
		_Scheduler_rawSend(proc, msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});

function _Scheduler_kill(proc)
{
	return _Scheduler_binding(function(callback) {
		var task = proc.f;
		if (task.$ === 2 && task.c)
		{
			task.c();
		}

		proc.f = null;

		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
}


/* STEP PROCESSES

type alias Process =
  { $ : tag
  , id : unique_id
  , root : Task
  , stack : null | { $: SUCCEED | FAIL, a: callback, b: stack }
  , mailbox : [msg]
  }

*/


var _Scheduler_working = false;
var _Scheduler_queue = [];


function _Scheduler_enqueue(proc)
{
	_Scheduler_queue.push(proc);
	if (_Scheduler_working)
	{
		return;
	}
	_Scheduler_working = true;
	while (proc = _Scheduler_queue.shift())
	{
		_Scheduler_step(proc);
	}
	_Scheduler_working = false;
}


function _Scheduler_step(proc)
{
	while (proc.f)
	{
		var rootTag = proc.f.$;
		if (rootTag === 0 || rootTag === 1)
		{
			while (proc.g && proc.g.$ !== rootTag)
			{
				proc.g = proc.g.i;
			}
			if (!proc.g)
			{
				return;
			}
			proc.f = proc.g.b(proc.f.a);
			proc.g = proc.g.i;
		}
		else if (rootTag === 2)
		{
			proc.f.c = proc.f.b(function(newRoot) {
				proc.f = newRoot;
				_Scheduler_enqueue(proc);
			});
			return;
		}
		else if (rootTag === 5)
		{
			if (proc.h.length === 0)
			{
				return;
			}
			proc.f = proc.f.b(proc.h.shift());
		}
		else // if (rootTag === 3 || rootTag === 4)
		{
			proc.g = {
				$: rootTag === 3 ? 0 : 1,
				b: proc.f.b,
				i: proc.g
			};
			proc.f = proc.f.d;
		}
	}
}



function _Process_sleep(time)
{
	return _Scheduler_binding(function(callback) {
		var id = setTimeout(function() {
			callback(_Scheduler_succeed(_Utils_Tuple0));
		}, time);

		return function() { clearTimeout(id); };
	});
}




// PROGRAMS


var _Platform_worker = F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.cu,
		impl.dq,
		impl.c5,
		function() { return function() {} }
	);
});



// INITIALIZE A PROGRAM


function _Platform_initialize(flagDecoder, args, init, update, subscriptions, stepperBuilder)
{
	var result = A2(_Json_run, flagDecoder, _Json_wrap(args ? args['flags'] : undefined));
	$elm$core$Result$isOk(result) || _Debug_crash(2 /**_UNUSED/, _Json_errorToString(result.a) /**/);
	var managers = {};
	var initPair = init(result.a);
	var model = initPair.a;
	var stepper = stepperBuilder(sendToApp, model);
	var ports = _Platform_setupEffects(managers, sendToApp);

	function sendToApp(msg, viewMetadata)
	{
		var pair = A2(update, msg, model);
		stepper(model = pair.a, viewMetadata);
		_Platform_enqueueEffects(managers, pair.b, subscriptions(model));
	}

	_Platform_enqueueEffects(managers, initPair.b, subscriptions(model));

	return ports ? { ports: ports } : {};
}



// TRACK PRELOADS
//
// This is used by code in elm/browser and elm/http
// to register any HTTP requests that are triggered by init.
//


var _Platform_preload;


function _Platform_registerPreload(url)
{
	_Platform_preload.add(url);
}



// EFFECT MANAGERS


var _Platform_effectManagers = {};


function _Platform_setupEffects(managers, sendToApp)
{
	var ports;

	// setup all necessary effect managers
	for (var key in _Platform_effectManagers)
	{
		var manager = _Platform_effectManagers[key];

		if (manager.a)
		{
			ports = ports || {};
			ports[key] = manager.a(key, sendToApp);
		}

		managers[key] = _Platform_instantiateManager(manager, sendToApp);
	}

	return ports;
}


function _Platform_createManager(init, onEffects, onSelfMsg, cmdMap, subMap)
{
	return {
		b: init,
		c: onEffects,
		d: onSelfMsg,
		e: cmdMap,
		f: subMap
	};
}


function _Platform_instantiateManager(info, sendToApp)
{
	var router = {
		g: sendToApp,
		h: undefined
	};

	var onEffects = info.c;
	var onSelfMsg = info.d;
	var cmdMap = info.e;
	var subMap = info.f;

	function loop(state)
	{
		return A2(_Scheduler_andThen, loop, _Scheduler_receive(function(msg)
		{
			var value = msg.a;

			if (msg.$ === 0)
			{
				return A3(onSelfMsg, router, value, state);
			}

			return cmdMap && subMap
				? A4(onEffects, router, value.i, value.j, state)
				: A3(onEffects, router, cmdMap ? value.i : value.j, state);
		}));
	}

	return router.h = _Scheduler_rawSpawn(A2(_Scheduler_andThen, loop, info.b));
}



// ROUTING


var _Platform_sendToApp = F2(function(router, msg)
{
	return _Scheduler_binding(function(callback)
	{
		router.g(msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});


var _Platform_sendToSelf = F2(function(router, msg)
{
	return A2(_Scheduler_send, router.h, {
		$: 0,
		a: msg
	});
});



// BAGS


function _Platform_leaf(home)
{
	return function(value)
	{
		return {
			$: 1,
			k: home,
			l: value
		};
	};
}


function _Platform_batch(list)
{
	return {
		$: 2,
		m: list
	};
}


var _Platform_map = F2(function(tagger, bag)
{
	return {
		$: 3,
		n: tagger,
		o: bag
	}
});



// PIPE BAGS INTO EFFECT MANAGERS
//
// Effects must be queued!
//
// Say your init contains a synchronous command, like Time.now or Time.here
//
//   - This will produce a batch of effects (FX_1)
//   - The synchronous task triggers the subsequent `update` call
//   - This will produce a batch of effects (FX_2)
//
// If we just start dispatching FX_2, subscriptions from FX_2 can be processed
// before subscriptions from FX_1. No good! Earlier versions of this code had
// this problem, leading to these reports:
//
//   https://github.com/elm/core/issues/980
//   https://github.com/elm/core/pull/981
//   https://github.com/elm/compiler/issues/1776
//
// The queue is necessary to avoid ordering issues for synchronous commands.


// Why use true/false here? Why not just check the length of the queue?
// The goal is to detect "are we currently dispatching effects?" If we
// are, we need to bail and let the ongoing while loop handle things.
//
// Now say the queue has 1 element. When we dequeue the final element,
// the queue will be empty, but we are still actively dispatching effects.
// So you could get queue jumping in a really tricky category of cases.
//
var _Platform_effectsQueue = [];
var _Platform_effectsActive = false;


function _Platform_enqueueEffects(managers, cmdBag, subBag)
{
	_Platform_effectsQueue.push({ p: managers, q: cmdBag, r: subBag });

	if (_Platform_effectsActive) return;

	_Platform_effectsActive = true;
	for (var fx; fx = _Platform_effectsQueue.shift(); )
	{
		_Platform_dispatchEffects(fx.p, fx.q, fx.r);
	}
	_Platform_effectsActive = false;
}


function _Platform_dispatchEffects(managers, cmdBag, subBag)
{
	var effectsDict = {};
	_Platform_gatherEffects(true, cmdBag, effectsDict, null);
	_Platform_gatherEffects(false, subBag, effectsDict, null);

	for (var home in managers)
	{
		_Scheduler_rawSend(managers[home], {
			$: 'fx',
			a: effectsDict[home] || { i: _List_Nil, j: _List_Nil }
		});
	}
}


function _Platform_gatherEffects(isCmd, bag, effectsDict, taggers)
{
	switch (bag.$)
	{
		case 1:
			var home = bag.k;
			var effect = _Platform_toEffect(isCmd, home, taggers, bag.l);
			effectsDict[home] = _Platform_insert(isCmd, effect, effectsDict[home]);
			return;

		case 2:
			for (var list = bag.m; list.b; list = list.b) // WHILE_CONS
			{
				_Platform_gatherEffects(isCmd, list.a, effectsDict, taggers);
			}
			return;

		case 3:
			_Platform_gatherEffects(isCmd, bag.o, effectsDict, {
				s: bag.n,
				t: taggers
			});
			return;
	}
}


function _Platform_toEffect(isCmd, home, taggers, value)
{
	function applyTaggers(x)
	{
		for (var temp = taggers; temp; temp = temp.t)
		{
			x = temp.s(x);
		}
		return x;
	}

	var map = isCmd
		? _Platform_effectManagers[home].e
		: _Platform_effectManagers[home].f;

	return A2(map, applyTaggers, value)
}


function _Platform_insert(isCmd, newEffect, effects)
{
	effects = effects || { i: _List_Nil, j: _List_Nil };

	isCmd
		? (effects.i = _List_Cons(newEffect, effects.i))
		: (effects.j = _List_Cons(newEffect, effects.j));

	return effects;
}



// PORTS


function _Platform_checkPortName(name)
{
	if (_Platform_effectManagers[name])
	{
		_Debug_crash(3, name)
	}
}



// OUTGOING PORTS


function _Platform_outgoingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		e: _Platform_outgoingPortMap,
		u: converter,
		a: _Platform_setupOutgoingPort
	};
	return _Platform_leaf(name);
}


var _Platform_outgoingPortMap = F2(function(tagger, value) { return value; });


function _Platform_setupOutgoingPort(name)
{
	var subs = [];
	var converter = _Platform_effectManagers[name].u;

	// CREATE MANAGER

	var init = _Process_sleep(0);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, cmdList, state)
	{
		for ( ; cmdList.b; cmdList = cmdList.b) // WHILE_CONS
		{
			// grab a separate reference to subs in case unsubscribe is called
			var currentSubs = subs;
			var value = _Json_unwrap(converter(cmdList.a));
			for (var i = 0; i < currentSubs.length; i++)
			{
				currentSubs[i](value);
			}
		}
		return init;
	});

	// PUBLIC API

	function subscribe(callback)
	{
		subs.push(callback);
	}

	function unsubscribe(callback)
	{
		// copy subs into a new array in case unsubscribe is called within a
		// subscribed callback
		subs = subs.slice();
		var index = subs.indexOf(callback);
		if (index >= 0)
		{
			subs.splice(index, 1);
		}
	}

	return {
		subscribe: subscribe,
		unsubscribe: unsubscribe
	};
}



// INCOMING PORTS


function _Platform_incomingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		f: _Platform_incomingPortMap,
		u: converter,
		a: _Platform_setupIncomingPort
	};
	return _Platform_leaf(name);
}


var _Platform_incomingPortMap = F2(function(tagger, finalTagger)
{
	return function(value)
	{
		return tagger(finalTagger(value));
	};
});


function _Platform_setupIncomingPort(name, sendToApp)
{
	var subs = _List_Nil;
	var converter = _Platform_effectManagers[name].u;

	// CREATE MANAGER

	var init = _Scheduler_succeed(null);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, subList, state)
	{
		subs = subList;
		return init;
	});

	// PUBLIC API

	function send(incomingValue)
	{
		var result = A2(_Json_run, converter, _Json_wrap(incomingValue));

		$elm$core$Result$isOk(result) || _Debug_crash(4, name, result.a);

		var value = result.a;
		for (var temp = subs; temp.b; temp = temp.b) // WHILE_CONS
		{
			sendToApp(temp.a(value));
		}
	}

	return { send: send };
}



// EXPORT ELM MODULES
//
// Have DEBUG and PROD versions so that we can (1) give nicer errors in
// debug mode and (2) not pay for the bits needed for that in prod mode.
//


function _Platform_export(exports)
{
	scope['Elm']
		? _Platform_mergeExportsProd(scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsProd(obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6)
				: _Platform_mergeExportsProd(obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}


function _Platform_export_UNUSED(exports)
{
	scope['Elm']
		? _Platform_mergeExportsDebug('Elm', scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsDebug(moduleName, obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6, moduleName)
				: _Platform_mergeExportsDebug(moduleName + '.' + name, obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}




// HELPERS


var _VirtualDom_divertHrefToApp;

var _VirtualDom_doc = typeof document !== 'undefined' ? document : {};


function _VirtualDom_appendChild(parent, child)
{
	parent.appendChild(child);
}

var _VirtualDom_init = F4(function(virtualNode, flagDecoder, debugMetadata, args)
{
	// NOTE: this function needs _Platform_export available to work

	/**/
	var node = args['node'];
	//*/
	/**_UNUSED/
	var node = args && args['node'] ? args['node'] : _Debug_crash(0);
	//*/

	node.parentNode.replaceChild(
		_VirtualDom_render(virtualNode, function() {}),
		node
	);

	return {};
});



// TEXT


function _VirtualDom_text(string)
{
	return {
		$: 0,
		a: string
	};
}



// NODE


var _VirtualDom_nodeNS = F2(function(namespace, tag)
{
	return F2(function(factList, kidList)
	{
		for (var kids = [], descendantsCount = 0; kidList.b; kidList = kidList.b) // WHILE_CONS
		{
			var kid = kidList.a;
			descendantsCount += (kid.b || 0);
			kids.push(kid);
		}
		descendantsCount += kids.length;

		return {
			$: 1,
			c: tag,
			d: _VirtualDom_organizeFacts(factList),
			e: kids,
			f: namespace,
			b: descendantsCount
		};
	});
});


var _VirtualDom_node = _VirtualDom_nodeNS(undefined);



// KEYED NODE


var _VirtualDom_keyedNodeNS = F2(function(namespace, tag)
{
	return F2(function(factList, kidList)
	{
		for (var kids = [], descendantsCount = 0; kidList.b; kidList = kidList.b) // WHILE_CONS
		{
			var kid = kidList.a;
			descendantsCount += (kid.b.b || 0);
			kids.push(kid);
		}
		descendantsCount += kids.length;

		return {
			$: 2,
			c: tag,
			d: _VirtualDom_organizeFacts(factList),
			e: kids,
			f: namespace,
			b: descendantsCount
		};
	});
});


var _VirtualDom_keyedNode = _VirtualDom_keyedNodeNS(undefined);



// CUSTOM


function _VirtualDom_custom(factList, model, render, diff)
{
	return {
		$: 3,
		d: _VirtualDom_organizeFacts(factList),
		g: model,
		h: render,
		i: diff
	};
}



// MAP


var _VirtualDom_map = F2(function(tagger, node)
{
	return {
		$: 4,
		j: tagger,
		k: node,
		b: 1 + (node.b || 0)
	};
});



// LAZY


function _VirtualDom_thunk(refs, thunk)
{
	return {
		$: 5,
		l: refs,
		m: thunk,
		k: undefined
	};
}

var _VirtualDom_lazy = F2(function(func, a)
{
	return _VirtualDom_thunk([func, a], function() {
		return func(a);
	});
});

var _VirtualDom_lazy2 = F3(function(func, a, b)
{
	return _VirtualDom_thunk([func, a, b], function() {
		return A2(func, a, b);
	});
});

var _VirtualDom_lazy3 = F4(function(func, a, b, c)
{
	return _VirtualDom_thunk([func, a, b, c], function() {
		return A3(func, a, b, c);
	});
});

var _VirtualDom_lazy4 = F5(function(func, a, b, c, d)
{
	return _VirtualDom_thunk([func, a, b, c, d], function() {
		return A4(func, a, b, c, d);
	});
});

var _VirtualDom_lazy5 = F6(function(func, a, b, c, d, e)
{
	return _VirtualDom_thunk([func, a, b, c, d, e], function() {
		return A5(func, a, b, c, d, e);
	});
});

var _VirtualDom_lazy6 = F7(function(func, a, b, c, d, e, f)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f], function() {
		return A6(func, a, b, c, d, e, f);
	});
});

var _VirtualDom_lazy7 = F8(function(func, a, b, c, d, e, f, g)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f, g], function() {
		return A7(func, a, b, c, d, e, f, g);
	});
});

var _VirtualDom_lazy8 = F9(function(func, a, b, c, d, e, f, g, h)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f, g, h], function() {
		return A8(func, a, b, c, d, e, f, g, h);
	});
});



// FACTS


var _VirtualDom_on = F2(function(key, handler)
{
	return {
		$: 'a0',
		n: key,
		o: handler
	};
});
var _VirtualDom_style = F2(function(key, value)
{
	return {
		$: 'a1',
		n: key,
		o: value
	};
});
var _VirtualDom_property = F2(function(key, value)
{
	return {
		$: 'a2',
		n: key,
		o: value
	};
});
var _VirtualDom_attribute = F2(function(key, value)
{
	return {
		$: 'a3',
		n: key,
		o: value
	};
});
var _VirtualDom_attributeNS = F3(function(namespace, key, value)
{
	return {
		$: 'a4',
		n: key,
		o: { f: namespace, o: value }
	};
});



// XSS ATTACK VECTOR CHECKS


function _VirtualDom_noScript(tag)
{
	return tag == 'script' ? 'p' : tag;
}

function _VirtualDom_noOnOrFormAction(key)
{
	return /^(on|formAction$)/i.test(key) ? 'data-' + key : key;
}

function _VirtualDom_noInnerHtmlOrFormAction(key)
{
	return key == 'innerHTML' || key == 'formAction' ? 'data-' + key : key;
}

function _VirtualDom_noJavaScriptUri(value)
{
	return /^javascript:/i.test(value.replace(/\s/g,'')) ? '' : value;
}

function _VirtualDom_noJavaScriptUri_UNUSED(value)
{
	return /^javascript:/i.test(value.replace(/\s/g,''))
		? 'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'
		: value;
}

function _VirtualDom_noJavaScriptOrHtmlUri(value)
{
	return /^\s*(javascript:|data:text\/html)/i.test(value) ? '' : value;
}

function _VirtualDom_noJavaScriptOrHtmlUri_UNUSED(value)
{
	return /^\s*(javascript:|data:text\/html)/i.test(value)
		? 'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'
		: value;
}



// MAP FACTS


var _VirtualDom_mapAttribute = F2(function(func, attr)
{
	return (attr.$ === 'a0')
		? A2(_VirtualDom_on, attr.n, _VirtualDom_mapHandler(func, attr.o))
		: attr;
});

function _VirtualDom_mapHandler(func, handler)
{
	var tag = $elm$virtual_dom$VirtualDom$toHandlerInt(handler);

	// 0 = Normal
	// 1 = MayStopPropagation
	// 2 = MayPreventDefault
	// 3 = Custom

	return {
		$: handler.$,
		a:
			!tag
				? A2($elm$json$Json$Decode$map, func, handler.a)
				:
			A3($elm$json$Json$Decode$map2,
				tag < 3
					? _VirtualDom_mapEventTuple
					: _VirtualDom_mapEventRecord,
				$elm$json$Json$Decode$succeed(func),
				handler.a
			)
	};
}

var _VirtualDom_mapEventTuple = F2(function(func, tuple)
{
	return _Utils_Tuple2(func(tuple.a), tuple.b);
});

var _VirtualDom_mapEventRecord = F2(function(func, record)
{
	return {
		w: func(record.w),
		aR: record.aR,
		aM: record.aM
	}
});



// ORGANIZE FACTS


function _VirtualDom_organizeFacts(factList)
{
	for (var facts = {}; factList.b; factList = factList.b) // WHILE_CONS
	{
		var entry = factList.a;

		var tag = entry.$;
		var key = entry.n;
		var value = entry.o;

		if (tag === 'a2')
		{
			(key === 'className')
				? _VirtualDom_addClass(facts, key, _Json_unwrap(value))
				: facts[key] = _Json_unwrap(value);

			continue;
		}

		var subFacts = facts[tag] || (facts[tag] = {});
		(tag === 'a3' && key === 'class')
			? _VirtualDom_addClass(subFacts, key, value)
			: subFacts[key] = value;
	}

	return facts;
}

function _VirtualDom_addClass(object, key, newClass)
{
	var classes = object[key];
	object[key] = classes ? classes + ' ' + newClass : newClass;
}



// RENDER


function _VirtualDom_render(vNode, eventNode)
{
	var tag = vNode.$;

	if (tag === 5)
	{
		return _VirtualDom_render(vNode.k || (vNode.k = vNode.m()), eventNode);
	}

	if (tag === 0)
	{
		return _VirtualDom_doc.createTextNode(vNode.a);
	}

	if (tag === 4)
	{
		var subNode = vNode.k;
		var tagger = vNode.j;

		while (subNode.$ === 4)
		{
			typeof tagger !== 'object'
				? tagger = [tagger, subNode.j]
				: tagger.push(subNode.j);

			subNode = subNode.k;
		}

		var subEventRoot = { j: tagger, p: eventNode };
		var domNode = _VirtualDom_render(subNode, subEventRoot);
		domNode.elm_event_node_ref = subEventRoot;
		return domNode;
	}

	if (tag === 3)
	{
		var domNode = vNode.h(vNode.g);
		_VirtualDom_applyFacts(domNode, eventNode, vNode.d);
		return domNode;
	}

	// at this point `tag` must be 1 or 2

	var domNode = vNode.f
		? _VirtualDom_doc.createElementNS(vNode.f, vNode.c)
		: _VirtualDom_doc.createElement(vNode.c);

	if (_VirtualDom_divertHrefToApp && vNode.c == 'a')
	{
		domNode.addEventListener('click', _VirtualDom_divertHrefToApp(domNode));
	}

	_VirtualDom_applyFacts(domNode, eventNode, vNode.d);

	for (var kids = vNode.e, i = 0; i < kids.length; i++)
	{
		_VirtualDom_appendChild(domNode, _VirtualDom_render(tag === 1 ? kids[i] : kids[i].b, eventNode));
	}

	return domNode;
}



// APPLY FACTS


function _VirtualDom_applyFacts(domNode, eventNode, facts)
{
	for (var key in facts)
	{
		var value = facts[key];

		key === 'a1'
			? _VirtualDom_applyStyles(domNode, value)
			:
		key === 'a0'
			? _VirtualDom_applyEvents(domNode, eventNode, value)
			:
		key === 'a3'
			? _VirtualDom_applyAttrs(domNode, value)
			:
		key === 'a4'
			? _VirtualDom_applyAttrsNS(domNode, value)
			:
		((key !== 'value' && key !== 'checked') || domNode[key] !== value) && (domNode[key] = value);
	}
}



// APPLY STYLES


function _VirtualDom_applyStyles(domNode, styles)
{
	var domNodeStyle = domNode.style;

	for (var key in styles)
	{
		domNodeStyle[key] = styles[key];
	}
}



// APPLY ATTRS


function _VirtualDom_applyAttrs(domNode, attrs)
{
	for (var key in attrs)
	{
		var value = attrs[key];
		typeof value !== 'undefined'
			? domNode.setAttribute(key, value)
			: domNode.removeAttribute(key);
	}
}



// APPLY NAMESPACED ATTRS


function _VirtualDom_applyAttrsNS(domNode, nsAttrs)
{
	for (var key in nsAttrs)
	{
		var pair = nsAttrs[key];
		var namespace = pair.f;
		var value = pair.o;

		typeof value !== 'undefined'
			? domNode.setAttributeNS(namespace, key, value)
			: domNode.removeAttributeNS(namespace, key);
	}
}



// APPLY EVENTS


function _VirtualDom_applyEvents(domNode, eventNode, events)
{
	var allCallbacks = domNode.elmFs || (domNode.elmFs = {});

	for (var key in events)
	{
		var newHandler = events[key];
		var oldCallback = allCallbacks[key];

		if (!newHandler)
		{
			domNode.removeEventListener(key, oldCallback);
			allCallbacks[key] = undefined;
			continue;
		}

		if (oldCallback)
		{
			var oldHandler = oldCallback.q;
			if (oldHandler.$ === newHandler.$)
			{
				oldCallback.q = newHandler;
				continue;
			}
			domNode.removeEventListener(key, oldCallback);
		}

		oldCallback = _VirtualDom_makeCallback(eventNode, newHandler);
		domNode.addEventListener(key, oldCallback,
			_VirtualDom_passiveSupported
			&& { passive: $elm$virtual_dom$VirtualDom$toHandlerInt(newHandler) < 2 }
		);
		allCallbacks[key] = oldCallback;
	}
}



// PASSIVE EVENTS


var _VirtualDom_passiveSupported;

try
{
	window.addEventListener('t', null, Object.defineProperty({}, 'passive', {
		get: function() { _VirtualDom_passiveSupported = true; }
	}));
}
catch(e) {}



// EVENT HANDLERS


function _VirtualDom_makeCallback(eventNode, initialHandler)
{
	function callback(event)
	{
		var handler = callback.q;
		var result = _Json_runHelp(handler.a, event);

		if (!$elm$core$Result$isOk(result))
		{
			return;
		}

		var tag = $elm$virtual_dom$VirtualDom$toHandlerInt(handler);

		// 0 = Normal
		// 1 = MayStopPropagation
		// 2 = MayPreventDefault
		// 3 = Custom

		var value = result.a;
		var message = !tag ? value : tag < 3 ? value.a : value.w;
		var stopPropagation = tag == 1 ? value.b : tag == 3 && value.aR;
		var currentEventNode = (
			stopPropagation && event.stopPropagation(),
			(tag == 2 ? value.b : tag == 3 && value.aM) && event.preventDefault(),
			eventNode
		);
		var tagger;
		var i;
		while (tagger = currentEventNode.j)
		{
			if (typeof tagger == 'function')
			{
				message = tagger(message);
			}
			else
			{
				for (var i = tagger.length; i--; )
				{
					message = tagger[i](message);
				}
			}
			currentEventNode = currentEventNode.p;
		}
		currentEventNode(message, stopPropagation); // stopPropagation implies isSync
	}

	callback.q = initialHandler;

	return callback;
}

function _VirtualDom_equalEvents(x, y)
{
	return x.$ == y.$ && _Json_equality(x.a, y.a);
}



// DIFF


// TODO: Should we do patches like in iOS?
//
// type Patch
//   = At Int Patch
//   | Batch (List Patch)
//   | Change ...
//
// How could it not be better?
//
function _VirtualDom_diff(x, y)
{
	var patches = [];
	_VirtualDom_diffHelp(x, y, patches, 0);
	return patches;
}


function _VirtualDom_pushPatch(patches, type, index, data)
{
	var patch = {
		$: type,
		r: index,
		s: data,
		t: undefined,
		u: undefined
	};
	patches.push(patch);
	return patch;
}


function _VirtualDom_diffHelp(x, y, patches, index)
{
	if (x === y)
	{
		return;
	}

	var xType = x.$;
	var yType = y.$;

	// Bail if you run into different types of nodes. Implies that the
	// structure has changed significantly and it's not worth a diff.
	if (xType !== yType)
	{
		if (xType === 1 && yType === 2)
		{
			y = _VirtualDom_dekey(y);
			yType = 1;
		}
		else
		{
			_VirtualDom_pushPatch(patches, 0, index, y);
			return;
		}
	}

	// Now we know that both nodes are the same $.
	switch (yType)
	{
		case 5:
			var xRefs = x.l;
			var yRefs = y.l;
			var i = xRefs.length;
			var same = i === yRefs.length;
			while (same && i--)
			{
				same = xRefs[i] === yRefs[i];
			}
			if (same)
			{
				y.k = x.k;
				return;
			}
			y.k = y.m();
			var subPatches = [];
			_VirtualDom_diffHelp(x.k, y.k, subPatches, 0);
			subPatches.length > 0 && _VirtualDom_pushPatch(patches, 1, index, subPatches);
			return;

		case 4:
			// gather nested taggers
			var xTaggers = x.j;
			var yTaggers = y.j;
			var nesting = false;

			var xSubNode = x.k;
			while (xSubNode.$ === 4)
			{
				nesting = true;

				typeof xTaggers !== 'object'
					? xTaggers = [xTaggers, xSubNode.j]
					: xTaggers.push(xSubNode.j);

				xSubNode = xSubNode.k;
			}

			var ySubNode = y.k;
			while (ySubNode.$ === 4)
			{
				nesting = true;

				typeof yTaggers !== 'object'
					? yTaggers = [yTaggers, ySubNode.j]
					: yTaggers.push(ySubNode.j);

				ySubNode = ySubNode.k;
			}

			// Just bail if different numbers of taggers. This implies the
			// structure of the virtual DOM has changed.
			if (nesting && xTaggers.length !== yTaggers.length)
			{
				_VirtualDom_pushPatch(patches, 0, index, y);
				return;
			}

			// check if taggers are "the same"
			if (nesting ? !_VirtualDom_pairwiseRefEqual(xTaggers, yTaggers) : xTaggers !== yTaggers)
			{
				_VirtualDom_pushPatch(patches, 2, index, yTaggers);
			}

			// diff everything below the taggers
			_VirtualDom_diffHelp(xSubNode, ySubNode, patches, index + 1);
			return;

		case 0:
			if (x.a !== y.a)
			{
				_VirtualDom_pushPatch(patches, 3, index, y.a);
			}
			return;

		case 1:
			_VirtualDom_diffNodes(x, y, patches, index, _VirtualDom_diffKids);
			return;

		case 2:
			_VirtualDom_diffNodes(x, y, patches, index, _VirtualDom_diffKeyedKids);
			return;

		case 3:
			if (x.h !== y.h)
			{
				_VirtualDom_pushPatch(patches, 0, index, y);
				return;
			}

			var factsDiff = _VirtualDom_diffFacts(x.d, y.d);
			factsDiff && _VirtualDom_pushPatch(patches, 4, index, factsDiff);

			var patch = y.i(x.g, y.g);
			patch && _VirtualDom_pushPatch(patches, 5, index, patch);

			return;
	}
}

// assumes the incoming arrays are the same length
function _VirtualDom_pairwiseRefEqual(as, bs)
{
	for (var i = 0; i < as.length; i++)
	{
		if (as[i] !== bs[i])
		{
			return false;
		}
	}

	return true;
}

function _VirtualDom_diffNodes(x, y, patches, index, diffKids)
{
	// Bail if obvious indicators have changed. Implies more serious
	// structural changes such that it's not worth it to diff.
	if (x.c !== y.c || x.f !== y.f)
	{
		_VirtualDom_pushPatch(patches, 0, index, y);
		return;
	}

	var factsDiff = _VirtualDom_diffFacts(x.d, y.d);
	factsDiff && _VirtualDom_pushPatch(patches, 4, index, factsDiff);

	diffKids(x, y, patches, index);
}



// DIFF FACTS


// TODO Instead of creating a new diff object, it's possible to just test if
// there *is* a diff. During the actual patch, do the diff again and make the
// modifications directly. This way, there's no new allocations. Worth it?
function _VirtualDom_diffFacts(x, y, category)
{
	var diff;

	// look for changes and removals
	for (var xKey in x)
	{
		if (xKey === 'a1' || xKey === 'a0' || xKey === 'a3' || xKey === 'a4')
		{
			var subDiff = _VirtualDom_diffFacts(x[xKey], y[xKey] || {}, xKey);
			if (subDiff)
			{
				diff = diff || {};
				diff[xKey] = subDiff;
			}
			continue;
		}

		// remove if not in the new facts
		if (!(xKey in y))
		{
			diff = diff || {};
			diff[xKey] =
				!category
					? (typeof x[xKey] === 'string' ? '' : null)
					:
				(category === 'a1')
					? ''
					:
				(category === 'a0' || category === 'a3')
					? undefined
					:
				{ f: x[xKey].f, o: undefined };

			continue;
		}

		var xValue = x[xKey];
		var yValue = y[xKey];

		// reference equal, so don't worry about it
		if (xValue === yValue && xKey !== 'value' && xKey !== 'checked'
			|| category === 'a0' && _VirtualDom_equalEvents(xValue, yValue))
		{
			continue;
		}

		diff = diff || {};
		diff[xKey] = yValue;
	}

	// add new stuff
	for (var yKey in y)
	{
		if (!(yKey in x))
		{
			diff = diff || {};
			diff[yKey] = y[yKey];
		}
	}

	return diff;
}



// DIFF KIDS


function _VirtualDom_diffKids(xParent, yParent, patches, index)
{
	var xKids = xParent.e;
	var yKids = yParent.e;

	var xLen = xKids.length;
	var yLen = yKids.length;

	// FIGURE OUT IF THERE ARE INSERTS OR REMOVALS

	if (xLen > yLen)
	{
		_VirtualDom_pushPatch(patches, 6, index, {
			v: yLen,
			i: xLen - yLen
		});
	}
	else if (xLen < yLen)
	{
		_VirtualDom_pushPatch(patches, 7, index, {
			v: xLen,
			e: yKids
		});
	}

	// PAIRWISE DIFF EVERYTHING ELSE

	for (var minLen = xLen < yLen ? xLen : yLen, i = 0; i < minLen; i++)
	{
		var xKid = xKids[i];
		_VirtualDom_diffHelp(xKid, yKids[i], patches, ++index);
		index += xKid.b || 0;
	}
}



// KEYED DIFF


function _VirtualDom_diffKeyedKids(xParent, yParent, patches, rootIndex)
{
	var localPatches = [];

	var changes = {}; // Dict String Entry
	var inserts = []; // Array { index : Int, entry : Entry }
	// type Entry = { tag : String, vnode : VNode, index : Int, data : _ }

	var xKids = xParent.e;
	var yKids = yParent.e;
	var xLen = xKids.length;
	var yLen = yKids.length;
	var xIndex = 0;
	var yIndex = 0;

	var index = rootIndex;

	while (xIndex < xLen && yIndex < yLen)
	{
		var x = xKids[xIndex];
		var y = yKids[yIndex];

		var xKey = x.a;
		var yKey = y.a;
		var xNode = x.b;
		var yNode = y.b;

		var newMatch = undefined;
		var oldMatch = undefined;

		// check if keys match

		if (xKey === yKey)
		{
			index++;
			_VirtualDom_diffHelp(xNode, yNode, localPatches, index);
			index += xNode.b || 0;

			xIndex++;
			yIndex++;
			continue;
		}

		// look ahead 1 to detect insertions and removals.

		var xNext = xKids[xIndex + 1];
		var yNext = yKids[yIndex + 1];

		if (xNext)
		{
			var xNextKey = xNext.a;
			var xNextNode = xNext.b;
			oldMatch = yKey === xNextKey;
		}

		if (yNext)
		{
			var yNextKey = yNext.a;
			var yNextNode = yNext.b;
			newMatch = xKey === yNextKey;
		}


		// swap x and y
		if (newMatch && oldMatch)
		{
			index++;
			_VirtualDom_diffHelp(xNode, yNextNode, localPatches, index);
			_VirtualDom_insertNode(changes, localPatches, xKey, yNode, yIndex, inserts);
			index += xNode.b || 0;

			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNextNode, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 2;
			continue;
		}

		// insert y
		if (newMatch)
		{
			index++;
			_VirtualDom_insertNode(changes, localPatches, yKey, yNode, yIndex, inserts);
			_VirtualDom_diffHelp(xNode, yNextNode, localPatches, index);
			index += xNode.b || 0;

			xIndex += 1;
			yIndex += 2;
			continue;
		}

		// remove x
		if (oldMatch)
		{
			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNode, index);
			index += xNode.b || 0;

			index++;
			_VirtualDom_diffHelp(xNextNode, yNode, localPatches, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 1;
			continue;
		}

		// remove x, insert y
		if (xNext && xNextKey === yNextKey)
		{
			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNode, index);
			_VirtualDom_insertNode(changes, localPatches, yKey, yNode, yIndex, inserts);
			index += xNode.b || 0;

			index++;
			_VirtualDom_diffHelp(xNextNode, yNextNode, localPatches, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 2;
			continue;
		}

		break;
	}

	// eat up any remaining nodes with removeNode and insertNode

	while (xIndex < xLen)
	{
		index++;
		var x = xKids[xIndex];
		var xNode = x.b;
		_VirtualDom_removeNode(changes, localPatches, x.a, xNode, index);
		index += xNode.b || 0;
		xIndex++;
	}

	while (yIndex < yLen)
	{
		var endInserts = endInserts || [];
		var y = yKids[yIndex];
		_VirtualDom_insertNode(changes, localPatches, y.a, y.b, undefined, endInserts);
		yIndex++;
	}

	if (localPatches.length > 0 || inserts.length > 0 || endInserts)
	{
		_VirtualDom_pushPatch(patches, 8, rootIndex, {
			w: localPatches,
			x: inserts,
			y: endInserts
		});
	}
}



// CHANGES FROM KEYED DIFF


var _VirtualDom_POSTFIX = '_elmW6BL';


function _VirtualDom_insertNode(changes, localPatches, key, vnode, yIndex, inserts)
{
	var entry = changes[key];

	// never seen this key before
	if (!entry)
	{
		entry = {
			c: 0,
			z: vnode,
			r: yIndex,
			s: undefined
		};

		inserts.push({ r: yIndex, A: entry });
		changes[key] = entry;

		return;
	}

	// this key was removed earlier, a match!
	if (entry.c === 1)
	{
		inserts.push({ r: yIndex, A: entry });

		entry.c = 2;
		var subPatches = [];
		_VirtualDom_diffHelp(entry.z, vnode, subPatches, entry.r);
		entry.r = yIndex;
		entry.s.s = {
			w: subPatches,
			A: entry
		};

		return;
	}

	// this key has already been inserted or moved, a duplicate!
	_VirtualDom_insertNode(changes, localPatches, key + _VirtualDom_POSTFIX, vnode, yIndex, inserts);
}


function _VirtualDom_removeNode(changes, localPatches, key, vnode, index)
{
	var entry = changes[key];

	// never seen this key before
	if (!entry)
	{
		var patch = _VirtualDom_pushPatch(localPatches, 9, index, undefined);

		changes[key] = {
			c: 1,
			z: vnode,
			r: index,
			s: patch
		};

		return;
	}

	// this key was inserted earlier, a match!
	if (entry.c === 0)
	{
		entry.c = 2;
		var subPatches = [];
		_VirtualDom_diffHelp(vnode, entry.z, subPatches, index);

		_VirtualDom_pushPatch(localPatches, 9, index, {
			w: subPatches,
			A: entry
		});

		return;
	}

	// this key has already been removed or moved, a duplicate!
	_VirtualDom_removeNode(changes, localPatches, key + _VirtualDom_POSTFIX, vnode, index);
}



// ADD DOM NODES
//
// Each DOM node has an "index" assigned in order of traversal. It is important
// to minimize our crawl over the actual DOM, so these indexes (along with the
// descendantsCount of virtual nodes) let us skip touching entire subtrees of
// the DOM if we know there are no patches there.


function _VirtualDom_addDomNodes(domNode, vNode, patches, eventNode)
{
	_VirtualDom_addDomNodesHelp(domNode, vNode, patches, 0, 0, vNode.b, eventNode);
}


// assumes `patches` is non-empty and indexes increase monotonically.
function _VirtualDom_addDomNodesHelp(domNode, vNode, patches, i, low, high, eventNode)
{
	var patch = patches[i];
	var index = patch.r;

	while (index === low)
	{
		var patchType = patch.$;

		if (patchType === 1)
		{
			_VirtualDom_addDomNodes(domNode, vNode.k, patch.s, eventNode);
		}
		else if (patchType === 8)
		{
			patch.t = domNode;
			patch.u = eventNode;

			var subPatches = patch.s.w;
			if (subPatches.length > 0)
			{
				_VirtualDom_addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
			}
		}
		else if (patchType === 9)
		{
			patch.t = domNode;
			patch.u = eventNode;

			var data = patch.s;
			if (data)
			{
				data.A.s = domNode;
				var subPatches = data.w;
				if (subPatches.length > 0)
				{
					_VirtualDom_addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
				}
			}
		}
		else
		{
			patch.t = domNode;
			patch.u = eventNode;
		}

		i++;

		if (!(patch = patches[i]) || (index = patch.r) > high)
		{
			return i;
		}
	}

	var tag = vNode.$;

	if (tag === 4)
	{
		var subNode = vNode.k;

		while (subNode.$ === 4)
		{
			subNode = subNode.k;
		}

		return _VirtualDom_addDomNodesHelp(domNode, subNode, patches, i, low + 1, high, domNode.elm_event_node_ref);
	}

	// tag must be 1 or 2 at this point

	var vKids = vNode.e;
	var childNodes = domNode.childNodes;
	for (var j = 0; j < vKids.length; j++)
	{
		low++;
		var vKid = tag === 1 ? vKids[j] : vKids[j].b;
		var nextLow = low + (vKid.b || 0);
		if (low <= index && index <= nextLow)
		{
			i = _VirtualDom_addDomNodesHelp(childNodes[j], vKid, patches, i, low, nextLow, eventNode);
			if (!(patch = patches[i]) || (index = patch.r) > high)
			{
				return i;
			}
		}
		low = nextLow;
	}
	return i;
}



// APPLY PATCHES


function _VirtualDom_applyPatches(rootDomNode, oldVirtualNode, patches, eventNode)
{
	if (patches.length === 0)
	{
		return rootDomNode;
	}

	_VirtualDom_addDomNodes(rootDomNode, oldVirtualNode, patches, eventNode);
	return _VirtualDom_applyPatchesHelp(rootDomNode, patches);
}

function _VirtualDom_applyPatchesHelp(rootDomNode, patches)
{
	for (var i = 0; i < patches.length; i++)
	{
		var patch = patches[i];
		var localDomNode = patch.t
		var newNode = _VirtualDom_applyPatch(localDomNode, patch);
		if (localDomNode === rootDomNode)
		{
			rootDomNode = newNode;
		}
	}
	return rootDomNode;
}

function _VirtualDom_applyPatch(domNode, patch)
{
	switch (patch.$)
	{
		case 0:
			return _VirtualDom_applyPatchRedraw(domNode, patch.s, patch.u);

		case 4:
			_VirtualDom_applyFacts(domNode, patch.u, patch.s);
			return domNode;

		case 3:
			domNode.replaceData(0, domNode.length, patch.s);
			return domNode;

		case 1:
			return _VirtualDom_applyPatchesHelp(domNode, patch.s);

		case 2:
			if (domNode.elm_event_node_ref)
			{
				domNode.elm_event_node_ref.j = patch.s;
			}
			else
			{
				domNode.elm_event_node_ref = { j: patch.s, p: patch.u };
			}
			return domNode;

		case 6:
			var data = patch.s;
			for (var i = 0; i < data.i; i++)
			{
				domNode.removeChild(domNode.childNodes[data.v]);
			}
			return domNode;

		case 7:
			var data = patch.s;
			var kids = data.e;
			var i = data.v;
			var theEnd = domNode.childNodes[i];
			for (; i < kids.length; i++)
			{
				domNode.insertBefore(_VirtualDom_render(kids[i], patch.u), theEnd);
			}
			return domNode;

		case 9:
			var data = patch.s;
			if (!data)
			{
				domNode.parentNode.removeChild(domNode);
				return domNode;
			}
			var entry = data.A;
			if (typeof entry.r !== 'undefined')
			{
				domNode.parentNode.removeChild(domNode);
			}
			entry.s = _VirtualDom_applyPatchesHelp(domNode, data.w);
			return domNode;

		case 8:
			return _VirtualDom_applyPatchReorder(domNode, patch);

		case 5:
			return patch.s(domNode);

		default:
			_Debug_crash(10); // 'Ran into an unknown patch!'
	}
}


function _VirtualDom_applyPatchRedraw(domNode, vNode, eventNode)
{
	var parentNode = domNode.parentNode;
	var newNode = _VirtualDom_render(vNode, eventNode);

	if (!newNode.elm_event_node_ref)
	{
		newNode.elm_event_node_ref = domNode.elm_event_node_ref;
	}

	if (parentNode && newNode !== domNode)
	{
		parentNode.replaceChild(newNode, domNode);
	}
	return newNode;
}


function _VirtualDom_applyPatchReorder(domNode, patch)
{
	var data = patch.s;

	// remove end inserts
	var frag = _VirtualDom_applyPatchReorderEndInsertsHelp(data.y, patch);

	// removals
	domNode = _VirtualDom_applyPatchesHelp(domNode, data.w);

	// inserts
	var inserts = data.x;
	for (var i = 0; i < inserts.length; i++)
	{
		var insert = inserts[i];
		var entry = insert.A;
		var node = entry.c === 2
			? entry.s
			: _VirtualDom_render(entry.z, patch.u);
		domNode.insertBefore(node, domNode.childNodes[insert.r]);
	}

	// add end inserts
	if (frag)
	{
		_VirtualDom_appendChild(domNode, frag);
	}

	return domNode;
}


function _VirtualDom_applyPatchReorderEndInsertsHelp(endInserts, patch)
{
	if (!endInserts)
	{
		return;
	}

	var frag = _VirtualDom_doc.createDocumentFragment();
	for (var i = 0; i < endInserts.length; i++)
	{
		var insert = endInserts[i];
		var entry = insert.A;
		_VirtualDom_appendChild(frag, entry.c === 2
			? entry.s
			: _VirtualDom_render(entry.z, patch.u)
		);
	}
	return frag;
}


function _VirtualDom_virtualize(node)
{
	// TEXT NODES

	if (node.nodeType === 3)
	{
		return _VirtualDom_text(node.textContent);
	}


	// WEIRD NODES

	if (node.nodeType !== 1)
	{
		return _VirtualDom_text('');
	}


	// ELEMENT NODES

	var attrList = _List_Nil;
	var attrs = node.attributes;
	for (var i = attrs.length; i--; )
	{
		var attr = attrs[i];
		var name = attr.name;
		var value = attr.value;
		attrList = _List_Cons( A2(_VirtualDom_attribute, name, value), attrList );
	}

	var tag = node.tagName.toLowerCase();
	var kidList = _List_Nil;
	var kids = node.childNodes;

	for (var i = kids.length; i--; )
	{
		kidList = _List_Cons(_VirtualDom_virtualize(kids[i]), kidList);
	}
	return A3(_VirtualDom_node, tag, attrList, kidList);
}

function _VirtualDom_dekey(keyedNode)
{
	var keyedKids = keyedNode.e;
	var len = keyedKids.length;
	var kids = new Array(len);
	for (var i = 0; i < len; i++)
	{
		kids[i] = keyedKids[i].b;
	}

	return {
		$: 1,
		c: keyedNode.c,
		d: keyedNode.d,
		e: kids,
		f: keyedNode.f,
		b: keyedNode.b
	};
}




// ELEMENT


var _Debugger_element;

var _Browser_element = _Debugger_element || F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.cu,
		impl.dq,
		impl.c5,
		function(sendToApp, initialModel) {
			var view = impl.dr;
			/**/
			var domNode = args['node'];
			//*/
			/**_UNUSED/
			var domNode = args && args['node'] ? args['node'] : _Debug_crash(0);
			//*/
			var currNode = _VirtualDom_virtualize(domNode);

			return _Browser_makeAnimator(initialModel, function(model)
			{
				var nextNode = view(model);
				var patches = _VirtualDom_diff(currNode, nextNode);
				domNode = _VirtualDom_applyPatches(domNode, currNode, patches, sendToApp);
				currNode = nextNode;
			});
		}
	);
});



// DOCUMENT


var _Debugger_document;

var _Browser_document = _Debugger_document || F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.cu,
		impl.dq,
		impl.c5,
		function(sendToApp, initialModel) {
			var divertHrefToApp = impl.aP && impl.aP(sendToApp)
			var view = impl.dr;
			var title = _VirtualDom_doc.title;
			var bodyNode = _VirtualDom_doc.body;
			var currNode = _VirtualDom_virtualize(bodyNode);
			return _Browser_makeAnimator(initialModel, function(model)
			{
				_VirtualDom_divertHrefToApp = divertHrefToApp;
				var doc = view(model);
				var nextNode = _VirtualDom_node('body')(_List_Nil)(doc.bY);
				var patches = _VirtualDom_diff(currNode, nextNode);
				bodyNode = _VirtualDom_applyPatches(bodyNode, currNode, patches, sendToApp);
				currNode = nextNode;
				_VirtualDom_divertHrefToApp = 0;
				(title !== doc.dl) && (_VirtualDom_doc.title = title = doc.dl);
			});
		}
	);
});



// ANIMATION


var _Browser_cancelAnimationFrame =
	typeof cancelAnimationFrame !== 'undefined'
		? cancelAnimationFrame
		: function(id) { clearTimeout(id); };

var _Browser_requestAnimationFrame =
	typeof requestAnimationFrame !== 'undefined'
		? requestAnimationFrame
		: function(callback) { return setTimeout(callback, 1000 / 60); };


function _Browser_makeAnimator(model, draw)
{
	draw(model);

	var state = 0;

	function updateIfNeeded()
	{
		state = state === 1
			? 0
			: ( _Browser_requestAnimationFrame(updateIfNeeded), draw(model), 1 );
	}

	return function(nextModel, isSync)
	{
		model = nextModel;

		isSync
			? ( draw(model),
				state === 2 && (state = 1)
				)
			: ( state === 0 && _Browser_requestAnimationFrame(updateIfNeeded),
				state = 2
				);
	};
}



// APPLICATION


function _Browser_application(impl)
{
	var onUrlChange = impl.cI;
	var onUrlRequest = impl.cJ;
	var key = function() { key.a(onUrlChange(_Browser_getUrl())); };

	return _Browser_document({
		aP: function(sendToApp)
		{
			key.a = sendToApp;
			_Browser_window.addEventListener('popstate', key);
			_Browser_window.navigator.userAgent.indexOf('Trident') < 0 || _Browser_window.addEventListener('hashchange', key);

			return F2(function(domNode, event)
			{
				if (!event.ctrlKey && !event.metaKey && !event.shiftKey && event.button < 1 && !domNode.target && !domNode.hasAttribute('download'))
				{
					event.preventDefault();
					var href = domNode.href;
					var curr = _Browser_getUrl();
					var next = $elm$url$Url$fromString(href).a;
					sendToApp(onUrlRequest(
						(next
							&& curr.bp === next.bp
							&& curr.a6 === next.a6
							&& curr.bm.a === next.bm.a
						)
							? $elm$browser$Browser$Internal(next)
							: $elm$browser$Browser$External(href)
					));
				}
			});
		},
		cu: function(flags)
		{
			return A3(impl.cu, flags, _Browser_getUrl(), key);
		},
		dr: impl.dr,
		dq: impl.dq,
		c5: impl.c5
	});
}

function _Browser_getUrl()
{
	return $elm$url$Url$fromString(_VirtualDom_doc.location.href).a || _Debug_crash(1);
}

var _Browser_go = F2(function(key, n)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		n && history.go(n);
		key();
	}));
});

var _Browser_pushUrl = F2(function(key, url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		history.pushState({}, '', url);
		key();
	}));
});

var _Browser_replaceUrl = F2(function(key, url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		history.replaceState({}, '', url);
		key();
	}));
});



// GLOBAL EVENTS


var _Browser_fakeNode = { addEventListener: function() {}, removeEventListener: function() {} };
var _Browser_doc = typeof document !== 'undefined' ? document : _Browser_fakeNode;
var _Browser_window = typeof window !== 'undefined' ? window : _Browser_fakeNode;

var _Browser_on = F3(function(node, eventName, sendToSelf)
{
	return _Scheduler_spawn(_Scheduler_binding(function(callback)
	{
		function handler(event)	{ _Scheduler_rawSpawn(sendToSelf(event)); }
		node.addEventListener(eventName, handler, _VirtualDom_passiveSupported && { passive: true });
		return function() { node.removeEventListener(eventName, handler); };
	}));
});

var _Browser_decodeEvent = F2(function(decoder, event)
{
	var result = _Json_runHelp(decoder, event);
	return $elm$core$Result$isOk(result) ? $elm$core$Maybe$Just(result.a) : $elm$core$Maybe$Nothing;
});



// PAGE VISIBILITY


function _Browser_visibilityInfo()
{
	return (typeof _VirtualDom_doc.hidden !== 'undefined')
		? { co: 'hidden', b6: 'visibilitychange' }
		:
	(typeof _VirtualDom_doc.mozHidden !== 'undefined')
		? { co: 'mozHidden', b6: 'mozvisibilitychange' }
		:
	(typeof _VirtualDom_doc.msHidden !== 'undefined')
		? { co: 'msHidden', b6: 'msvisibilitychange' }
		:
	(typeof _VirtualDom_doc.webkitHidden !== 'undefined')
		? { co: 'webkitHidden', b6: 'webkitvisibilitychange' }
		: { co: 'hidden', b6: 'visibilitychange' };
}



// ANIMATION FRAMES


function _Browser_rAF()
{
	return _Scheduler_binding(function(callback)
	{
		var id = _Browser_requestAnimationFrame(function() {
			callback(_Scheduler_succeed(Date.now()));
		});

		return function() {
			_Browser_cancelAnimationFrame(id);
		};
	});
}


function _Browser_now()
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(Date.now()));
	});
}



// DOM STUFF


function _Browser_withNode(id, doStuff)
{
	return _Scheduler_binding(function(callback)
	{
		_Browser_requestAnimationFrame(function() {
			var node = document.getElementById(id);
			callback(node
				? _Scheduler_succeed(doStuff(node))
				: _Scheduler_fail($elm$browser$Browser$Dom$NotFound(id))
			);
		});
	});
}


function _Browser_withWindow(doStuff)
{
	return _Scheduler_binding(function(callback)
	{
		_Browser_requestAnimationFrame(function() {
			callback(_Scheduler_succeed(doStuff()));
		});
	});
}


// FOCUS and BLUR


var _Browser_call = F2(function(functionName, id)
{
	return _Browser_withNode(id, function(node) {
		node[functionName]();
		return _Utils_Tuple0;
	});
});



// WINDOW VIEWPORT


function _Browser_getViewport()
{
	return {
		bu: _Browser_getScene(),
		bB: {
			bE: _Browser_window.pageXOffset,
			bF: _Browser_window.pageYOffset,
			al: _Browser_doc.documentElement.clientWidth,
			aG: _Browser_doc.documentElement.clientHeight
		}
	};
}

function _Browser_getScene()
{
	var body = _Browser_doc.body;
	var elem = _Browser_doc.documentElement;
	return {
		al: Math.max(body.scrollWidth, body.offsetWidth, elem.scrollWidth, elem.offsetWidth, elem.clientWidth),
		aG: Math.max(body.scrollHeight, body.offsetHeight, elem.scrollHeight, elem.offsetHeight, elem.clientHeight)
	};
}

var _Browser_setViewport = F2(function(x, y)
{
	return _Browser_withWindow(function()
	{
		_Browser_window.scroll(x, y);
		return _Utils_Tuple0;
	});
});



// ELEMENT VIEWPORT


function _Browser_getViewportOf(id)
{
	return _Browser_withNode(id, function(node)
	{
		return {
			bu: {
				al: node.scrollWidth,
				aG: node.scrollHeight
			},
			bB: {
				bE: node.scrollLeft,
				bF: node.scrollTop,
				al: node.clientWidth,
				aG: node.clientHeight
			}
		};
	});
}


var _Browser_setViewportOf = F3(function(id, x, y)
{
	return _Browser_withNode(id, function(node)
	{
		node.scrollLeft = x;
		node.scrollTop = y;
		return _Utils_Tuple0;
	});
});



// ELEMENT


function _Browser_getElement(id)
{
	return _Browser_withNode(id, function(node)
	{
		var rect = node.getBoundingClientRect();
		var x = _Browser_window.pageXOffset;
		var y = _Browser_window.pageYOffset;
		return {
			bu: _Browser_getScene(),
			bB: {
				bE: x,
				bF: y,
				al: _Browser_doc.documentElement.clientWidth,
				aG: _Browser_doc.documentElement.clientHeight
			},
			ci: {
				bE: x + rect.left,
				bF: y + rect.top,
				al: rect.width,
				aG: rect.height
			}
		};
	});
}



// LOAD and RELOAD


function _Browser_reload(skipCache)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function(callback)
	{
		_VirtualDom_doc.location.reload(skipCache);
	}));
}

function _Browser_load(url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function(callback)
	{
		try
		{
			_Browser_window.location = url;
		}
		catch(err)
		{
			// Only Firefox can throw a NS_ERROR_MALFORMED_URI exception here.
			// Other browsers reload the page, so let's be consistent about that.
			_VirtualDom_doc.location.reload(false);
		}
	}));
}



var _Bitwise_and = F2(function(a, b)
{
	return a & b;
});

var _Bitwise_or = F2(function(a, b)
{
	return a | b;
});

var _Bitwise_xor = F2(function(a, b)
{
	return a ^ b;
});

function _Bitwise_complement(a)
{
	return ~a;
};

var _Bitwise_shiftLeftBy = F2(function(offset, a)
{
	return a << offset;
});

var _Bitwise_shiftRightBy = F2(function(offset, a)
{
	return a >> offset;
});

var _Bitwise_shiftRightZfBy = F2(function(offset, a)
{
	return a >>> offset;
});



function _Time_now(millisToPosix)
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(millisToPosix(Date.now())));
	});
}

var _Time_setInterval = F2(function(interval, task)
{
	return _Scheduler_binding(function(callback)
	{
		var id = setInterval(function() { _Scheduler_rawSpawn(task); }, interval);
		return function() { clearInterval(id); };
	});
});

function _Time_here()
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(
			A2($elm$time$Time$customZone, -(new Date().getTimezoneOffset()), _List_Nil)
		));
	});
}


function _Time_getZoneName()
{
	return _Scheduler_binding(function(callback)
	{
		try
		{
			var name = $elm$time$Time$Name(Intl.DateTimeFormat().resolvedOptions().timeZone);
		}
		catch (e)
		{
			var name = $elm$time$Time$Offset(new Date().getTimezoneOffset());
		}
		callback(_Scheduler_succeed(name));
	});
}
var $elm$core$Basics$EQ = 1;
var $elm$core$Basics$GT = 2;
var $elm$core$Basics$LT = 0;
var $elm$core$List$cons = _List_cons;
var $elm$core$Dict$foldr = F3(
	function (func, acc, t) {
		foldr:
		while (true) {
			if (t.$ === -2) {
				return acc;
			} else {
				var key = t.b;
				var value = t.c;
				var left = t.d;
				var right = t.e;
				var $temp$func = func,
					$temp$acc = A3(
					func,
					key,
					value,
					A3($elm$core$Dict$foldr, func, acc, right)),
					$temp$t = left;
				func = $temp$func;
				acc = $temp$acc;
				t = $temp$t;
				continue foldr;
			}
		}
	});
var $elm$core$Dict$toList = function (dict) {
	return A3(
		$elm$core$Dict$foldr,
		F3(
			function (key, value, list) {
				return A2(
					$elm$core$List$cons,
					_Utils_Tuple2(key, value),
					list);
			}),
		_List_Nil,
		dict);
};
var $elm$core$Dict$keys = function (dict) {
	return A3(
		$elm$core$Dict$foldr,
		F3(
			function (key, value, keyList) {
				return A2($elm$core$List$cons, key, keyList);
			}),
		_List_Nil,
		dict);
};
var $elm$core$Set$toList = function (_v0) {
	var dict = _v0;
	return $elm$core$Dict$keys(dict);
};
var $elm$core$Elm$JsArray$foldr = _JsArray_foldr;
var $elm$core$Array$foldr = F3(
	function (func, baseCase, _v0) {
		var tree = _v0.c;
		var tail = _v0.d;
		var helper = F2(
			function (node, acc) {
				if (!node.$) {
					var subTree = node.a;
					return A3($elm$core$Elm$JsArray$foldr, helper, acc, subTree);
				} else {
					var values = node.a;
					return A3($elm$core$Elm$JsArray$foldr, func, acc, values);
				}
			});
		return A3(
			$elm$core$Elm$JsArray$foldr,
			helper,
			A3($elm$core$Elm$JsArray$foldr, func, baseCase, tail),
			tree);
	});
var $elm$core$Array$toList = function (array) {
	return A3($elm$core$Array$foldr, $elm$core$List$cons, _List_Nil, array);
};
var $elm$core$Result$Err = function (a) {
	return {$: 1, a: a};
};
var $elm$json$Json$Decode$Failure = F2(
	function (a, b) {
		return {$: 3, a: a, b: b};
	});
var $elm$json$Json$Decode$Field = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $elm$json$Json$Decode$Index = F2(
	function (a, b) {
		return {$: 1, a: a, b: b};
	});
var $elm$core$Result$Ok = function (a) {
	return {$: 0, a: a};
};
var $elm$json$Json$Decode$OneOf = function (a) {
	return {$: 2, a: a};
};
var $elm$core$Basics$False = 1;
var $elm$core$Basics$add = _Basics_add;
var $elm$core$Maybe$Just = function (a) {
	return {$: 0, a: a};
};
var $elm$core$Maybe$Nothing = {$: 1};
var $elm$core$String$all = _String_all;
var $elm$core$Basics$and = _Basics_and;
var $elm$core$Basics$append = _Utils_append;
var $elm$json$Json$Encode$encode = _Json_encode;
var $elm$core$String$fromInt = _String_fromNumber;
var $elm$core$String$join = F2(
	function (sep, chunks) {
		return A2(
			_String_join,
			sep,
			_List_toArray(chunks));
	});
var $elm$core$String$split = F2(
	function (sep, string) {
		return _List_fromArray(
			A2(_String_split, sep, string));
	});
var $elm$json$Json$Decode$indent = function (str) {
	return A2(
		$elm$core$String$join,
		'\n    ',
		A2($elm$core$String$split, '\n', str));
};
var $elm$core$List$foldl = F3(
	function (func, acc, list) {
		foldl:
		while (true) {
			if (!list.b) {
				return acc;
			} else {
				var x = list.a;
				var xs = list.b;
				var $temp$func = func,
					$temp$acc = A2(func, x, acc),
					$temp$list = xs;
				func = $temp$func;
				acc = $temp$acc;
				list = $temp$list;
				continue foldl;
			}
		}
	});
var $elm$core$List$length = function (xs) {
	return A3(
		$elm$core$List$foldl,
		F2(
			function (_v0, i) {
				return i + 1;
			}),
		0,
		xs);
};
var $elm$core$List$map2 = _List_map2;
var $elm$core$Basics$le = _Utils_le;
var $elm$core$Basics$sub = _Basics_sub;
var $elm$core$List$rangeHelp = F3(
	function (lo, hi, list) {
		rangeHelp:
		while (true) {
			if (_Utils_cmp(lo, hi) < 1) {
				var $temp$lo = lo,
					$temp$hi = hi - 1,
					$temp$list = A2($elm$core$List$cons, hi, list);
				lo = $temp$lo;
				hi = $temp$hi;
				list = $temp$list;
				continue rangeHelp;
			} else {
				return list;
			}
		}
	});
var $elm$core$List$range = F2(
	function (lo, hi) {
		return A3($elm$core$List$rangeHelp, lo, hi, _List_Nil);
	});
var $elm$core$List$indexedMap = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$map2,
			f,
			A2(
				$elm$core$List$range,
				0,
				$elm$core$List$length(xs) - 1),
			xs);
	});
var $elm$core$Char$toCode = _Char_toCode;
var $elm$core$Char$isLower = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (97 <= code) && (code <= 122);
};
var $elm$core$Char$isUpper = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (code <= 90) && (65 <= code);
};
var $elm$core$Basics$or = _Basics_or;
var $elm$core$Char$isAlpha = function (_char) {
	return $elm$core$Char$isLower(_char) || $elm$core$Char$isUpper(_char);
};
var $elm$core$Char$isDigit = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (code <= 57) && (48 <= code);
};
var $elm$core$Char$isAlphaNum = function (_char) {
	return $elm$core$Char$isLower(_char) || ($elm$core$Char$isUpper(_char) || $elm$core$Char$isDigit(_char));
};
var $elm$core$List$reverse = function (list) {
	return A3($elm$core$List$foldl, $elm$core$List$cons, _List_Nil, list);
};
var $elm$core$String$uncons = _String_uncons;
var $elm$json$Json$Decode$errorOneOf = F2(
	function (i, error) {
		return '\n\n(' + ($elm$core$String$fromInt(i + 1) + (') ' + $elm$json$Json$Decode$indent(
			$elm$json$Json$Decode$errorToString(error))));
	});
var $elm$json$Json$Decode$errorToString = function (error) {
	return A2($elm$json$Json$Decode$errorToStringHelp, error, _List_Nil);
};
var $elm$json$Json$Decode$errorToStringHelp = F2(
	function (error, context) {
		errorToStringHelp:
		while (true) {
			switch (error.$) {
				case 0:
					var f = error.a;
					var err = error.b;
					var isSimple = function () {
						var _v1 = $elm$core$String$uncons(f);
						if (_v1.$ === 1) {
							return false;
						} else {
							var _v2 = _v1.a;
							var _char = _v2.a;
							var rest = _v2.b;
							return $elm$core$Char$isAlpha(_char) && A2($elm$core$String$all, $elm$core$Char$isAlphaNum, rest);
						}
					}();
					var fieldName = isSimple ? ('.' + f) : ('[\'' + (f + '\']'));
					var $temp$error = err,
						$temp$context = A2($elm$core$List$cons, fieldName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 1:
					var i = error.a;
					var err = error.b;
					var indexName = '[' + ($elm$core$String$fromInt(i) + ']');
					var $temp$error = err,
						$temp$context = A2($elm$core$List$cons, indexName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 2:
					var errors = error.a;
					if (!errors.b) {
						return 'Ran into a Json.Decode.oneOf with no possibilities' + function () {
							if (!context.b) {
								return '!';
							} else {
								return ' at json' + A2(
									$elm$core$String$join,
									'',
									$elm$core$List$reverse(context));
							}
						}();
					} else {
						if (!errors.b.b) {
							var err = errors.a;
							var $temp$error = err,
								$temp$context = context;
							error = $temp$error;
							context = $temp$context;
							continue errorToStringHelp;
						} else {
							var starter = function () {
								if (!context.b) {
									return 'Json.Decode.oneOf';
								} else {
									return 'The Json.Decode.oneOf at json' + A2(
										$elm$core$String$join,
										'',
										$elm$core$List$reverse(context));
								}
							}();
							var introduction = starter + (' failed in the following ' + ($elm$core$String$fromInt(
								$elm$core$List$length(errors)) + ' ways:'));
							return A2(
								$elm$core$String$join,
								'\n\n',
								A2(
									$elm$core$List$cons,
									introduction,
									A2($elm$core$List$indexedMap, $elm$json$Json$Decode$errorOneOf, errors)));
						}
					}
				default:
					var msg = error.a;
					var json = error.b;
					var introduction = function () {
						if (!context.b) {
							return 'Problem with the given value:\n\n';
						} else {
							return 'Problem with the value at json' + (A2(
								$elm$core$String$join,
								'',
								$elm$core$List$reverse(context)) + ':\n\n    ');
						}
					}();
					return introduction + ($elm$json$Json$Decode$indent(
						A2($elm$json$Json$Encode$encode, 4, json)) + ('\n\n' + msg));
			}
		}
	});
var $elm$core$Array$branchFactor = 32;
var $elm$core$Array$Array_elm_builtin = F4(
	function (a, b, c, d) {
		return {$: 0, a: a, b: b, c: c, d: d};
	});
var $elm$core$Elm$JsArray$empty = _JsArray_empty;
var $elm$core$Basics$ceiling = _Basics_ceiling;
var $elm$core$Basics$fdiv = _Basics_fdiv;
var $elm$core$Basics$logBase = F2(
	function (base, number) {
		return _Basics_log(number) / _Basics_log(base);
	});
var $elm$core$Basics$toFloat = _Basics_toFloat;
var $elm$core$Array$shiftStep = $elm$core$Basics$ceiling(
	A2($elm$core$Basics$logBase, 2, $elm$core$Array$branchFactor));
var $elm$core$Array$empty = A4($elm$core$Array$Array_elm_builtin, 0, $elm$core$Array$shiftStep, $elm$core$Elm$JsArray$empty, $elm$core$Elm$JsArray$empty);
var $elm$core$Elm$JsArray$initialize = _JsArray_initialize;
var $elm$core$Array$Leaf = function (a) {
	return {$: 1, a: a};
};
var $elm$core$Basics$apL = F2(
	function (f, x) {
		return f(x);
	});
var $elm$core$Basics$apR = F2(
	function (x, f) {
		return f(x);
	});
var $elm$core$Basics$eq = _Utils_equal;
var $elm$core$Basics$floor = _Basics_floor;
var $elm$core$Elm$JsArray$length = _JsArray_length;
var $elm$core$Basics$gt = _Utils_gt;
var $elm$core$Basics$max = F2(
	function (x, y) {
		return (_Utils_cmp(x, y) > 0) ? x : y;
	});
var $elm$core$Basics$mul = _Basics_mul;
var $elm$core$Array$SubTree = function (a) {
	return {$: 0, a: a};
};
var $elm$core$Elm$JsArray$initializeFromList = _JsArray_initializeFromList;
var $elm$core$Array$compressNodes = F2(
	function (nodes, acc) {
		compressNodes:
		while (true) {
			var _v0 = A2($elm$core$Elm$JsArray$initializeFromList, $elm$core$Array$branchFactor, nodes);
			var node = _v0.a;
			var remainingNodes = _v0.b;
			var newAcc = A2(
				$elm$core$List$cons,
				$elm$core$Array$SubTree(node),
				acc);
			if (!remainingNodes.b) {
				return $elm$core$List$reverse(newAcc);
			} else {
				var $temp$nodes = remainingNodes,
					$temp$acc = newAcc;
				nodes = $temp$nodes;
				acc = $temp$acc;
				continue compressNodes;
			}
		}
	});
var $elm$core$Tuple$first = function (_v0) {
	var x = _v0.a;
	return x;
};
var $elm$core$Array$treeFromBuilder = F2(
	function (nodeList, nodeListSize) {
		treeFromBuilder:
		while (true) {
			var newNodeSize = $elm$core$Basics$ceiling(nodeListSize / $elm$core$Array$branchFactor);
			if (newNodeSize === 1) {
				return A2($elm$core$Elm$JsArray$initializeFromList, $elm$core$Array$branchFactor, nodeList).a;
			} else {
				var $temp$nodeList = A2($elm$core$Array$compressNodes, nodeList, _List_Nil),
					$temp$nodeListSize = newNodeSize;
				nodeList = $temp$nodeList;
				nodeListSize = $temp$nodeListSize;
				continue treeFromBuilder;
			}
		}
	});
var $elm$core$Array$builderToArray = F2(
	function (reverseNodeList, builder) {
		if (!builder.c) {
			return A4(
				$elm$core$Array$Array_elm_builtin,
				$elm$core$Elm$JsArray$length(builder.e),
				$elm$core$Array$shiftStep,
				$elm$core$Elm$JsArray$empty,
				builder.e);
		} else {
			var treeLen = builder.c * $elm$core$Array$branchFactor;
			var depth = $elm$core$Basics$floor(
				A2($elm$core$Basics$logBase, $elm$core$Array$branchFactor, treeLen - 1));
			var correctNodeList = reverseNodeList ? $elm$core$List$reverse(builder.f) : builder.f;
			var tree = A2($elm$core$Array$treeFromBuilder, correctNodeList, builder.c);
			return A4(
				$elm$core$Array$Array_elm_builtin,
				$elm$core$Elm$JsArray$length(builder.e) + treeLen,
				A2($elm$core$Basics$max, 5, depth * $elm$core$Array$shiftStep),
				tree,
				builder.e);
		}
	});
var $elm$core$Basics$idiv = _Basics_idiv;
var $elm$core$Basics$lt = _Utils_lt;
var $elm$core$Array$initializeHelp = F5(
	function (fn, fromIndex, len, nodeList, tail) {
		initializeHelp:
		while (true) {
			if (fromIndex < 0) {
				return A2(
					$elm$core$Array$builderToArray,
					false,
					{f: nodeList, c: (len / $elm$core$Array$branchFactor) | 0, e: tail});
			} else {
				var leaf = $elm$core$Array$Leaf(
					A3($elm$core$Elm$JsArray$initialize, $elm$core$Array$branchFactor, fromIndex, fn));
				var $temp$fn = fn,
					$temp$fromIndex = fromIndex - $elm$core$Array$branchFactor,
					$temp$len = len,
					$temp$nodeList = A2($elm$core$List$cons, leaf, nodeList),
					$temp$tail = tail;
				fn = $temp$fn;
				fromIndex = $temp$fromIndex;
				len = $temp$len;
				nodeList = $temp$nodeList;
				tail = $temp$tail;
				continue initializeHelp;
			}
		}
	});
var $elm$core$Basics$remainderBy = _Basics_remainderBy;
var $elm$core$Array$initialize = F2(
	function (len, fn) {
		if (len <= 0) {
			return $elm$core$Array$empty;
		} else {
			var tailLen = len % $elm$core$Array$branchFactor;
			var tail = A3($elm$core$Elm$JsArray$initialize, tailLen, len - tailLen, fn);
			var initialFromIndex = (len - tailLen) - $elm$core$Array$branchFactor;
			return A5($elm$core$Array$initializeHelp, fn, initialFromIndex, len, _List_Nil, tail);
		}
	});
var $elm$core$Basics$True = 0;
var $elm$core$Result$isOk = function (result) {
	if (!result.$) {
		return true;
	} else {
		return false;
	}
};
var $elm$json$Json$Decode$map = _Json_map1;
var $elm$json$Json$Decode$map2 = _Json_map2;
var $elm$json$Json$Decode$succeed = _Json_succeed;
var $elm$virtual_dom$VirtualDom$toHandlerInt = function (handler) {
	switch (handler.$) {
		case 0:
			return 0;
		case 1:
			return 1;
		case 2:
			return 2;
		default:
			return 3;
	}
};
var $elm$browser$Browser$External = function (a) {
	return {$: 1, a: a};
};
var $elm$browser$Browser$Internal = function (a) {
	return {$: 0, a: a};
};
var $elm$core$Basics$identity = function (x) {
	return x;
};
var $elm$browser$Browser$Dom$NotFound = $elm$core$Basics$identity;
var $elm$url$Url$Http = 0;
var $elm$url$Url$Https = 1;
var $elm$url$Url$Url = F6(
	function (protocol, host, port_, path, query, fragment) {
		return {a2: fragment, a6: host, bk: path, bm: port_, bp: protocol, bq: query};
	});
var $elm$core$String$contains = _String_contains;
var $elm$core$String$length = _String_length;
var $elm$core$String$slice = _String_slice;
var $elm$core$String$dropLeft = F2(
	function (n, string) {
		return (n < 1) ? string : A3(
			$elm$core$String$slice,
			n,
			$elm$core$String$length(string),
			string);
	});
var $elm$core$String$indexes = _String_indexes;
var $elm$core$String$isEmpty = function (string) {
	return string === '';
};
var $elm$core$String$left = F2(
	function (n, string) {
		return (n < 1) ? '' : A3($elm$core$String$slice, 0, n, string);
	});
var $elm$core$String$toInt = _String_toInt;
var $elm$url$Url$chompBeforePath = F5(
	function (protocol, path, params, frag, str) {
		if ($elm$core$String$isEmpty(str) || A2($elm$core$String$contains, '@', str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, ':', str);
			if (!_v0.b) {
				return $elm$core$Maybe$Just(
					A6($elm$url$Url$Url, protocol, str, $elm$core$Maybe$Nothing, path, params, frag));
			} else {
				if (!_v0.b.b) {
					var i = _v0.a;
					var _v1 = $elm$core$String$toInt(
						A2($elm$core$String$dropLeft, i + 1, str));
					if (_v1.$ === 1) {
						return $elm$core$Maybe$Nothing;
					} else {
						var port_ = _v1;
						return $elm$core$Maybe$Just(
							A6(
								$elm$url$Url$Url,
								protocol,
								A2($elm$core$String$left, i, str),
								port_,
								path,
								params,
								frag));
					}
				} else {
					return $elm$core$Maybe$Nothing;
				}
			}
		}
	});
var $elm$url$Url$chompBeforeQuery = F4(
	function (protocol, params, frag, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '/', str);
			if (!_v0.b) {
				return A5($elm$url$Url$chompBeforePath, protocol, '/', params, frag, str);
			} else {
				var i = _v0.a;
				return A5(
					$elm$url$Url$chompBeforePath,
					protocol,
					A2($elm$core$String$dropLeft, i, str),
					params,
					frag,
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$url$Url$chompBeforeFragment = F3(
	function (protocol, frag, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '?', str);
			if (!_v0.b) {
				return A4($elm$url$Url$chompBeforeQuery, protocol, $elm$core$Maybe$Nothing, frag, str);
			} else {
				var i = _v0.a;
				return A4(
					$elm$url$Url$chompBeforeQuery,
					protocol,
					$elm$core$Maybe$Just(
						A2($elm$core$String$dropLeft, i + 1, str)),
					frag,
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$url$Url$chompAfterProtocol = F2(
	function (protocol, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '#', str);
			if (!_v0.b) {
				return A3($elm$url$Url$chompBeforeFragment, protocol, $elm$core$Maybe$Nothing, str);
			} else {
				var i = _v0.a;
				return A3(
					$elm$url$Url$chompBeforeFragment,
					protocol,
					$elm$core$Maybe$Just(
						A2($elm$core$String$dropLeft, i + 1, str)),
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$core$String$startsWith = _String_startsWith;
var $elm$url$Url$fromString = function (str) {
	return A2($elm$core$String$startsWith, 'http://', str) ? A2(
		$elm$url$Url$chompAfterProtocol,
		0,
		A2($elm$core$String$dropLeft, 7, str)) : (A2($elm$core$String$startsWith, 'https://', str) ? A2(
		$elm$url$Url$chompAfterProtocol,
		1,
		A2($elm$core$String$dropLeft, 8, str)) : $elm$core$Maybe$Nothing);
};
var $elm$core$Basics$never = function (_v0) {
	never:
	while (true) {
		var nvr = _v0;
		var $temp$_v0 = nvr;
		_v0 = $temp$_v0;
		continue never;
	}
};
var $elm$core$Task$Perform = $elm$core$Basics$identity;
var $elm$core$Task$succeed = _Scheduler_succeed;
var $elm$core$Task$init = $elm$core$Task$succeed(0);
var $elm$core$List$foldrHelper = F4(
	function (fn, acc, ctr, ls) {
		if (!ls.b) {
			return acc;
		} else {
			var a = ls.a;
			var r1 = ls.b;
			if (!r1.b) {
				return A2(fn, a, acc);
			} else {
				var b = r1.a;
				var r2 = r1.b;
				if (!r2.b) {
					return A2(
						fn,
						a,
						A2(fn, b, acc));
				} else {
					var c = r2.a;
					var r3 = r2.b;
					if (!r3.b) {
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(fn, c, acc)));
					} else {
						var d = r3.a;
						var r4 = r3.b;
						var res = (ctr > 500) ? A3(
							$elm$core$List$foldl,
							fn,
							acc,
							$elm$core$List$reverse(r4)) : A4($elm$core$List$foldrHelper, fn, acc, ctr + 1, r4);
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(
									fn,
									c,
									A2(fn, d, res))));
					}
				}
			}
		}
	});
var $elm$core$List$foldr = F3(
	function (fn, acc, ls) {
		return A4($elm$core$List$foldrHelper, fn, acc, 0, ls);
	});
var $elm$core$List$map = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$foldr,
			F2(
				function (x, acc) {
					return A2(
						$elm$core$List$cons,
						f(x),
						acc);
				}),
			_List_Nil,
			xs);
	});
var $elm$core$Task$andThen = _Scheduler_andThen;
var $elm$core$Task$map = F2(
	function (func, taskA) {
		return A2(
			$elm$core$Task$andThen,
			function (a) {
				return $elm$core$Task$succeed(
					func(a));
			},
			taskA);
	});
var $elm$core$Task$map2 = F3(
	function (func, taskA, taskB) {
		return A2(
			$elm$core$Task$andThen,
			function (a) {
				return A2(
					$elm$core$Task$andThen,
					function (b) {
						return $elm$core$Task$succeed(
							A2(func, a, b));
					},
					taskB);
			},
			taskA);
	});
var $elm$core$Task$sequence = function (tasks) {
	return A3(
		$elm$core$List$foldr,
		$elm$core$Task$map2($elm$core$List$cons),
		$elm$core$Task$succeed(_List_Nil),
		tasks);
};
var $elm$core$Platform$sendToApp = _Platform_sendToApp;
var $elm$core$Task$spawnCmd = F2(
	function (router, _v0) {
		var task = _v0;
		return _Scheduler_spawn(
			A2(
				$elm$core$Task$andThen,
				$elm$core$Platform$sendToApp(router),
				task));
	});
var $elm$core$Task$onEffects = F3(
	function (router, commands, state) {
		return A2(
			$elm$core$Task$map,
			function (_v0) {
				return 0;
			},
			$elm$core$Task$sequence(
				A2(
					$elm$core$List$map,
					$elm$core$Task$spawnCmd(router),
					commands)));
	});
var $elm$core$Task$onSelfMsg = F3(
	function (_v0, _v1, _v2) {
		return $elm$core$Task$succeed(0);
	});
var $elm$core$Task$cmdMap = F2(
	function (tagger, _v0) {
		var task = _v0;
		return A2($elm$core$Task$map, tagger, task);
	});
_Platform_effectManagers['Task'] = _Platform_createManager($elm$core$Task$init, $elm$core$Task$onEffects, $elm$core$Task$onSelfMsg, $elm$core$Task$cmdMap);
var $elm$core$Task$command = _Platform_leaf('Task');
var $elm$core$Task$perform = F2(
	function (toMessage, task) {
		return $elm$core$Task$command(
			A2($elm$core$Task$map, toMessage, task));
	});
var $elm$browser$Browser$document = _Browser_document;
var $elm$core$Platform$Cmd$batch = _Platform_batch;
var $elm$core$Platform$Cmd$none = $elm$core$Platform$Cmd$batch(_List_Nil);
var $elm$json$Json$Decode$decodeValue = _Json_run;
var $author$project$Main$defaultDimensions = {aG: 500, al: 500};
var $author$project$Main$Dimensions = F2(
	function (width, height) {
		return {aG: height, al: width};
	});
var $elm$json$Json$Decode$field = _Json_decodeField;
var $elm$json$Json$Decode$int = _Json_decodeInt;
var $author$project$Main$dimensionsDecoder = A3(
	$elm$json$Json$Decode$map2,
	$author$project$Main$Dimensions,
	A2($elm$json$Json$Decode$field, 'width', $elm$json$Json$Decode$int),
	A2($elm$json$Json$Decode$field, 'height', $elm$json$Json$Decode$int));
var $elm$core$Result$withDefault = F2(
	function (def, result) {
		if (!result.$) {
			var a = result.a;
			return a;
		} else {
			return def;
		}
	});
var $author$project$Main$parseFlags = function (flags) {
	return A2(
		$elm$core$Result$withDefault,
		$author$project$Main$defaultDimensions,
		A2($elm$json$Json$Decode$decodeValue, $author$project$Main$dimensionsDecoder, flags));
};
var $author$project$Words$words = _List_fromArray(
	['abacus', 'abased', 'abaser', 'abase', 'abated', 'abater', 'abate', 'abbacy', 'abbess', 'abbey', 'abbot', 'abduct', 'abhor', 'abided', 'abider', 'abide', 'abject', 'abjure', 'ablaze', 'ablest', 'abloom', 'aboard', 'abode', 'abort', 'abound', 'abrade', 'abroad', 'abrupt', 'absent', 'absorb', 'absurd', 'abused', 'abuser', 'abuse', 'abysm', 'acacia', 'accede', 'accent', 'accept', 'access', 'accord', 'accost', 'accrue', 'accuse', 'acetic', 'acetyl', 'aching', 'acidic', 'acidly', 'acorn', 'acquit', 'across', 'acting', 'action', 'active', 'actor', 'actual', 'acuity', 'acumen', 'adage', 'adagio', 'adapt', 'addend', 'adder', 'addict', 'adding', 'addled', 'addle', 'adduce', 'adduct', 'adept', 'adhere', 'adidas', 'adieu', 'adieux', 'adjoin', 'adjure', 'adjust', 'admire', 'admit', 'adobe', 'adonis', 'adopt', 'adored', 'adorer', 'adore', 'adorn', 'adrift', 'adroit', 'adsorb', 'adult', 'advent', 'adverb', 'advert', 'advice', 'advise', 'aerate', 'aerie', 'aerial', 'aerobe', 'affair', 'affect', 'affirm', 'afford', 'affray', 'afghan', 'afield', 'aflame', 'afloat', 'afraid', 'afresh', 'africa', 'agate', 'ageism', 'agency', 'agenda', 'agent', 'aghast', 'agleam', 'agnate', 'agonic', 'agorae', 'agouti', 'agree', 'agreed', 'aiding', 'ailing', 'aiming', 'airbag', 'airbus', 'airier', 'airily', 'airing', 'airmen', 'airman', 'airway', 'aisle', 'akimbo', 'alarm', 'alaska', 'albany', 'albedo', 'albeit', 'albino', 'album', 'alcove', 'alder', 'aleck', 'alert', 'alexia', 'alibi', 'alien', 'alight', 'align', 'alkali', 'alkyd', 'alkyl', 'allay', 'allele', 'allege', 'alley', 'allied', 'allies', 'allots', 'allows', 'alloys', 'allude', 'allure', 'almond', 'almost', 'alnico', 'alohas', 'alpaca', 'alphas', 'alpine', 'altars', 'alters', 'althea', 'alumna', 'alumni', 'always', 'amazes', 'amazed', 'amazon', 'ambers', 'ambits', 'ambled', 'ambler', 'ambles', 'ambush', 'amebic', 'amebas', 'amends', 'amerce', 'amices', 'amides', 'amidst', 'amigos', 'amines', 'amnion', 'amoral', 'amount', 'amours', 'ampere', 'ampler', 'amtrak', 'amulet', 'amuses', 'amused', 'analog', 'anchor', 'ancien', 'anemia', 'anemic', 'angels', 'angers', 'angina', 'angled', 'angler', 'angles', 'angola', 'angora', 'angsts', 'animas', 'animal', 'animus', 'anions', 'anises', 'ankara', 'ankles', 'anklet', 'annals', 'anneal', 'annoys', 'annual', 'annuls', 'anodes', 'anodic', 'anodal', 'anoint', 'anomie', 'anoxia', 'anoxic', 'answer', 'anther', 'anthem', 'antics', 'antler', 'anuses', 'anvils', 'anyhow', 'anyone', 'anyway', 'aortic', 'aortal', 'aortas', 'aoudad', 'apathy', 'apexes', 'aphids', 'apiary', 'apices', 'apical', 'apiece', 'aplomb', 'apogee', 'apollo', 'appall', 'appeal', 'appear', 'append', 'apples', 'appose', 'aprons', 'aptest', 'arabic', 'arable', 'arabia', 'arbors', 'arcade', 'arcane', 'arched', 'arches', 'archly', 'archer', 'arcing', 'arctic', 'ardent', 'ardors', 'arenas', 'argent', 'argons', 'argosy', 'argots', 'argued', 'arguer', 'argues', 'argyle', 'aright', 'arises', 'arisen', 'armada', 'armful', 'armies', 'arming', 'armlet', 'armors', 'armory', 'armpit', 'arnica', 'aromas', 'around', 'arouse', 'arrant', 'arrays', 'arrest', 'arrive', 'arrows', 'arroyo', 'arsons', 'artery', 'artful', 'artier', 'artist', 'aryans', 'ascend', 'ascent', 'ascots', 'ashore', 'asians', 'asides', 'asking', 'aslant', 'asleep', 'aspect', 'aspens', 'aspics', 'aspire', 'assail', 'assays', 'assent', 'assert', 'assess', 'assets', 'assign', 'assist', 'assort', 'assume', 'assure', 'asters', 'astern', 'asthma', 'astral', 'astray', 'astute', 'asylum', 'ataxic', 'ataxia', 'athens', 'atolls', 'atomic', 'atoned', 'atonal', 'atones', 'atrium', 'attach', 'attack', 'attain', 'attars', 'attend', 'attest', 'attics', 'attire', 'attune', 'audios', 'audits', 'augers', 'augurs', 'augury', 'august', 'aurora', 'austin', 'author', 'autism', 'autumn', 'auxins', 'avails', 'avatar', 'avenge', 'avenue', 'averse', 'averts', 'aviary', 'avidly', 'avocet', 'avoids', 'avouch', 'avowal', 'avowed', 'awaits', 'awaked', 'awakes', 'awaken', 'awards', 'aweigh', 'awhile', 'awning', 'awoken', 'axioms', 'azalea', 'aztecs', 'azures', 'babels', 'babied', 'babies', 'baboon', 'backed', 'backer', 'backup', 'bacons', 'badger', 'badges', 'baffle', 'bagels', 'bagged', 'bahama', 'bailed', 'bailer', 'bairns', 'baited', 'baizes', 'bakers', 'bakery', 'baking', 'balder', 'baldly', 'baleen', 'balers', 'baling', 'balked', 'balkan', 'balled', 'ballad', 'ballet', 'ballot', 'balsas', 'balsam', 'baltic', 'bamboo', 'banana', 'banded', 'bandit', 'banged', 'bangle', 'banish', 'banjos', 'banked', 'banker', 'banned', 'banner', 'banter', 'bantam', 'banyan', 'baobab', 'barbed', 'barber', 'barbel', 'barely', 'barest', 'barged', 'barges', 'baring', 'barite', 'barium', 'barked', 'barker', 'barley', 'barmen', 'barman', 'barons', 'barony', 'barque', 'barred', 'barrel', 'barren', 'barrio', 'barrow', 'barter', 'baryon', 'basalt', 'basest', 'bashed', 'bashes', 'basics', 'basins', 'basing', 'basked', 'basket', 'basque', 'basses', 'basset', 'bassos', 'basted', 'bastes', 'batboy', 'bateau', 'bathed', 'bather', 'bathes', 'bathos', 'batiks', 'bating', 'batons', 'batted', 'batten', 'batter', 'battle', 'bauble', 'bawled', 'baying', 'bayous', 'bazaar', 'beacon', 'beaded', 'beadle', 'beagle', 'beaked', 'beaker', 'beamed', 'beaned', 'beanie', 'bearer', 'beards', 'beasts', 'beater', 'beaten', 'beauts', 'beauty', 'beaver', 'becalm', 'became', 'beckon', 'become', 'bedaub', 'bedbug', 'bedded', 'bedeck', 'bedews', 'bedlam', 'bedpan', 'beefed', 'beeped', 'beeper', 'beetle', 'beeves', 'befall', 'befell', 'befits', 'befogs', 'before', 'befoul', 'begets', 'begged', 'beggar', 'begins', 'begird', 'begirt', 'behalf', 'behave', 'behead', 'beheld', 'behest', 'behind', 'behold', 'beiges', 'beings', 'beirut', 'belays', 'beldam', 'belfry', 'belied', 'belies', 'belief', 'belize', 'belled', 'belles', 'bellow', 'belong', 'belows', 'belted', 'beluga', 'bemata', 'bemire', 'bemoan', 'bemuse', 'bender', 'benign', 'benumb', 'benzol', 'berate', 'berber', 'bereft', 'berets', 'berlin', 'berths', 'bertha', 'beryls', 'beseem', 'besets', 'beside', 'besots', 'bested', 'bestir', 'bestow', 'betake', 'betels', 'betide', 'betook', 'betray', 'bettor', 'better', 'bevels', 'bevies', 'bewail', 'beware', 'beyond', 'bezels', 'bhutan', 'bialys', 'biased', 'biases', 'bibles', 'biceps', 'bicker', 'bidder', 'bidden', 'bidets', 'biding', 'bigamy', 'bigger', 'bights', 'bigots', 'bigwig', 'bijoux', 'bikers', 'biking', 'bikini', 'bilged', 'bilges', 'bilked', 'billed', 'billet', 'billow', 'binary', 'binder', 'binges', 'bingos', 'biomes', 'biopsy', 'biotic', 'biotas', 'biotin', 'bipeds', 'birdie', 'births', 'bisect', 'bishop', 'bisque', 'bissau', 'bistro', 'bitchy', 'biters', 'biting', 'bitten', 'bitter', 'blabby', 'blacks', 'bladed', 'blades', 'blamed', 'blames', 'blanch', 'blanks', 'blared', 'blares', 'blasts', 'blazed', 'blazer', 'blazes', 'blazon', 'bleach', 'blears', 'bleary', 'bleats', 'bleeds', 'bleeps', 'blench', 'blends', 'blight', 'blimps', 'blinds', 'blinks', 'blintz', 'blithe', 'bloats', 'blocks', 'blokes', 'blonds', 'blonde', 'bloods', 'bloody', 'blooms', 'blotch', 'blouse', 'blower', 'blowsy', 'blowup', 'blowzy', 'bluest', 'bluets', 'bluffs', 'bluing', 'bluish', 'blunts', 'blurbs', 'blurry', 'blurts', 'boards', 'boasts', 'boated', 'boater', 'boatel', 'bobbed', 'bobble', 'bobbin', 'bobcat', 'bodies', 'bodily', 'bodice', 'boding', 'bodkin', 'bogeys', 'bogged', 'boggle', 'bogies', 'bogota', 'boiled', 'boiler', 'bolder', 'boldly', 'bolero', 'bollix', 'bolted', 'bombed', 'bomber', 'bonbon', 'bonded', 'boners', 'bonged', 'bongos', 'bonier', 'boning', 'bonito', 'bonnet', 'bonsai', 'boodle', 'boohoo', 'booing', 'booked', 'booker', 'bookie', 'boomed', 'boosts', 'booted', 'bootee', 'booths', 'boozed', 'boozes', 'bopped', 'borate', 'border', 'boreal', 'borers', 'boring', 'borneo', 'borons', 'borrow', 'borzoi', 'bosoms', 'bosons', 'bossed', 'bosses', 'boston', 'botany', 'botfly', 'bother', 'bottle', 'bottom', 'boughs', 'bought', 'bounce', 'bouncy', 'bounds', 'bounty', 'bourns', 'bourse', 'bovine', 'bowels', 'bowers', 'bowfin', 'bowies', 'bowing', 'bowled', 'bowler', 'bowleg', 'boxcar', 'boxers', 'boxier', 'boxing', 'boyish', 'braced', 'bracer', 'braces', 'bracts', 'brahma', 'braids', 'brains', 'brainy', 'braise', 'braked', 'brakes', 'branch', 'brands', 'brandy', 'brassy', 'bratty', 'braved', 'braves', 'braver', 'bravos', 'brawls', 'brawns', 'brawny', 'brayed', 'brayer', 'brazed', 'brazes', 'brazen', 'brazil', 'breach', 'breads', 'breaks', 'breast', 'breath', 'breech', 'breeds', 'breeze', 'breezy', 'breton', 'brevet', 'brewed', 'brewer', 'briars', 'bribed', 'bribes', 'bricks', 'brides', 'bridal', 'bridle', 'bridge', 'briefs', 'briers', 'bright', 'brines', 'brings', 'brinks', 'briton', 'broach', 'brogan', 'brogue', 'broils', 'broker', 'broken', 'bronco', 'bronze', 'brooch', 'broods', 'broody', 'brooks', 'brooms', 'broths', 'browns', 'browse', 'bruins', 'bruise', 'bruits', 'brumes', 'brunch', 'brunei', 'brunet', 'brunts', 'brutes', 'brutal', 'bubble', 'buboes', 'bucked', 'buckle', 'bucket', 'budded', 'budder', 'buddha', 'budged', 'budges', 'budget', 'budgie', 'buenos', 'buffed', 'buffer', 'buffet', 'bugged', 'bugled', 'bugler', 'bugles', 'builds', 'bulbar', 'bulbul', 'bulged', 'bulges', 'bulked', 'bulled', 'bullet', 'bumble', 'bummed', 'bumped', 'bumper', 'buncos', 'bundle', 'bunged', 'bungle', 'bunion', 'bunked', 'bunker', 'bunkum', 'bunsen', 'bunted', 'buoyed', 'burble', 'burden', 'bureau', 'burger', 'burgle', 'burghs', 'buried', 'buries', 'burial', 'burins', 'burled', 'burlap', 'burned', 'burner', 'burped', 'burred', 'burros', 'burrow', 'bursae', 'bursar', 'bursts', 'busboy', 'bushed', 'bushes', 'bushel', 'busied', 'busier', 'busies', 'busily', 'busing', 'buskin', 'bussed', 'busses', 'busted', 'buster', 'bustle', 'butane', 'butler', 'butted', 'buttes', 'butter', 'button', 'butyls', 'buyers', 'buying', 'buzzed', 'buzzer', 'buzzes', 'bygone', 'bylaws', 'bypass', 'byroad', 'byssus', 'byways', 'byword', 'cabals', 'cabana', 'cabbed', 'cabins', 'cabled', 'cables', 'cacaos', 'cached', 'caches', 'cachet', 'cackly', 'cackle', 'cactus', 'caddie', 'caddis', 'cadets', 'cadged', 'cadges', 'cadmic', 'cadres', 'caesar', 'caftan', 'cagier', 'cagily', 'caging', 'caiman', 'cairns', 'cajole', 'caking', 'calico', 'caliph', 'called', 'caller', 'callow', 'callus', 'calmed', 'calmer', 'calmly', 'calved', 'calves', 'camber', 'camels', 'cameos', 'camera', 'camped', 'camper', 'campus', 'canada', 'canals', 'canape', 'canary', 'canard', 'cancan', 'cancel', 'cancer', 'candor', 'candle', 'candid', 'canine', 'caning', 'canker', 'canned', 'cannel', 'cannon', 'cannot', 'canoes', 'canoed', 'canons', 'canopy', 'canted', 'canter', 'cantor', 'cantle', 'canthi', 'cantos', 'canton', 'canvas', 'canyon', 'capers', 'caping', 'capita', 'capons', 'capped', 'captor', 'carafe', 'carats', 'carbon', 'carboy', 'carded', 'career', 'careen', 'caress', 'carets', 'carhop', 'caries', 'caring', 'carnal', 'carobs', 'carols', 'caroms', 'carped', 'carpal', 'carpel', 'carpet', 'carpus', 'carrel', 'carrot', 'carted', 'cartel', 'carton', 'carved', 'carver', 'carves', 'casaba', 'casein', 'cashed', 'cashes', 'cashew', 'casing', 'casino', 'casket', 'cassia', 'caster', 'castes', 'castor', 'castle', 'casual', 'catchy', 'caters', 'catgut', 'cation', 'catkin', 'catnap', 'catnip', 'cattle', 'caucus', 'caudal', 'caught', 'caulks', 'caused', 'causes', 'causal', 'caveat', 'cavern', 'caviar', 'cavils', 'caving', 'cavity', 'cavort', 'cawing', 'cayuse', 'ceased', 'ceases', 'cedars', 'ceding', 'celery', 'celiac', 'celled', 'cellar', 'cellos', 'celtic', 'cement', 'censer', 'censor', 'census', 'center', 'cereal', 'cerise', 'cerium', 'cermet', 'cervix', 'cesium', 'cetera', 'ceylon', 'chafed', 'chafes', 'chaffs', 'chains', 'chairs', 'chaise', 'chalet', 'chalks', 'chalky', 'champs', 'chance', 'chancy', 'change', 'chants', 'chapel', 'chards', 'charge', 'charms', 'charts', 'chased', 'chaser', 'chases', 'chasms', 'chasse', 'chaste', 'chatty', 'cheats', 'checks', 'cheeks', 'cheeky', 'cheeps', 'cheers', 'cheery', 'cheese', 'cheesy', 'chemin', 'cherry', 'cherub', 'chests', 'chewed', 'chicle', 'chicks', 'chided', 'chides', 'chiefs', 'chills', 'chilly', 'chimed', 'chimes', 'chimps', 'chinas', 'chinch', 'chines', 'chinks', 'chinos', 'chintz', 'chirps', 'chisel', 'chitin', 'chiton', 'chives', 'chocks', 'choice', 'choirs', 'choked', 'choker', 'chokes', 'choler', 'choose', 'choosy', 'choppy', 'choric', 'choral', 'chords', 'chores', 'chorea', 'chorus', 'chosen', 'chrism', 'christ', 'chrome', 'chubby', 'chucks', 'chukka', 'chummy', 'chumps', 'chunks', 'chunky', 'church', 'churls', 'churns', 'chutes', 'cicada', 'ciders', 'cigars', 'cilium', 'cinder', 'cinema', 'cipher', 'circle', 'circus', 'cirque', 'cirrus', 'cities', 'citify', 'citing', 'citric', 'citron', 'citrus', 'civets', 'civics', 'clacks', 'claims', 'clammy', 'clamor', 'clamps', 'clangs', 'clanks', 'claque', 'claret', 'clasps', 'classy', 'clause', 'clawed', 'clayey', 'cleans', 'clears', 'cleats', 'cleave', 'clefts', 'clench', 'clergy', 'cleric', 'clerks', 'clever', 'cliche', 'clicks', 'client', 'cliffs', 'climes', 'climax', 'climbs', 'clines', 'clinch', 'clings', 'clinic', 'clinks', 'clique', 'cloaca', 'clorks', 'cloche', 'clocks', 'cloned', 'clones', 'closed', 'closer', 'closes', 'closet', 'cloths', 'clothe', 'clouds', 'cloudy', 'clouts', 'clover', 'cloves', 'cloven', 'clowns', 'cloyed', 'clubby', 'clucks', 'clumps', 'clumsy', 'clutch', 'coarse', 'coasts', 'coated', 'coatis', 'coaxed', 'coaxes', 'cobalt', 'cobble', 'cobras', 'cobweb', 'coccus', 'coccyx', 'cocked', 'cocker', 'cockle', 'cocoas', 'cocoon', 'coddle', 'codger', 'codify', 'coding', 'coerce', 'coeval', 'coffer', 'coffee', 'coffin', 'cogent', 'cognac', 'cohere', 'cohort', 'coifed', 'coiled', 'coined', 'coitus', 'colder', 'coldly', 'coleus', 'colics', 'collar', 'collie', 'colons', 'colony', 'colors', 'column', 'combed', 'combat', 'combos', 'comely', 'comedy', 'comedo', 'comers', 'comets', 'comfit', 'comics', 'coming', 'comity', 'commas', 'commit', 'commix', 'common', 'compel', 'comply', 'comsat', 'conchs', 'concur', 'condor', 'condom', 'confab', 'confer', 'congas', 'conger', 'conies', 'conked', 'conned', 'consul', 'contra', 'convex', 'convey', 'convoy', 'cooing', 'cooked', 'cooker', 'cookie', 'cooled', 'cooler', 'coolly', 'coolie', 'cooped', 'cooper', 'cootie', 'copals', 'copied', 'copier', 'copies', 'coping', 'copped', 'copper', 'copras', 'copses', 'copter', 'copula', 'corals', 'corbel', 'corded', 'cordon', 'corers', 'coring', 'corium', 'corked', 'corker', 'corned', 'corner', 'cornea', 'cornet', 'corona', 'corpse', 'corpus', 'corral', 'corset', 'cortex', 'coryza', 'cosign', 'cosine', 'cosmic', 'cosmos', 'cosset', 'costly', 'costal', 'costae', 'costar', 'cotter', 'cotton', 'cougar', 'coughs', 'coulee', 'counts', 'county', 'coupes', 'couple', 'coupon', 'course', 'courts', 'cousin', 'covens', 'covers', 'covert', 'covets', 'coveys', 'coward', 'cowboy', 'cowers', 'cowing', 'cowled', 'cowpox', 'coyest', 'coyote', 'cozens', 'cozier', 'cozily', 'crabby', 'cracks', 'cradle', 'crafts', 'crafty', 'craggy', 'cramps', 'craned', 'cranes', 'cranks', 'cranky', 'cranny', 'crated', 'crater', 'crates', 'craved', 'craves', 'cravat', 'craven', 'crawly', 'crawls', 'crayon', 'crazed', 'crazes', 'creaks', 'creaky', 'creams', 'creamy', 'crease', 'create', 'creche', 'credit', 'credos', 'creeds', 'creeks', 'creels', 'creeps', 'creepy', 'cremes', 'creole', 'crepes', 'cresol', 'crests', 'cretan', 'cretin', 'crewed', 'crewel', 'cricks', 'criers', 'crimes', 'crimea', 'crimps', 'cringe', 'crises', 'crisis', 'crisps', 'crispy', 'critic', 'croaks', 'croats', 'crocks', 'crocus', 'crofts', 'crones', 'crooks', 'croons', 'crouch', 'croups', 'crowed', 'crowds', 'crowns', 'cruder', 'crudes', 'cruddy', 'cruets', 'cruise', 'crumbs', 'crummy', 'crunch', 'cruses', 'crusts', 'crusty', 'crutch', 'cruxes', 'crying', 'crypts', 'cubans', 'cubing', 'cubism', 'cubist', 'cubits', 'cuckoo', 'cuddly', 'cuddle', 'cudgel', 'cuffed', 'culled', 'cultic', 'cumber', 'cumins', 'cumuli', 'cupels', 'cupful', 'cupids', 'cupola', 'cupped', 'cupric', 'curare', 'curate', 'curbed', 'curdle', 'curfew', 'curies', 'curiae', 'curing', 'curios', 'curium', 'curled', 'curler', 'curlew', 'cursed', 'curser', 'curses', 'cursor', 'curter', 'curtly', 'curtsy', 'curves', 'curved', 'curvet', 'cuspid', 'cussed', 'cusses', 'custom', 'cutest', 'cutlet', 'cutoff', 'cutout', 'cutter', 'cutups', 'cyanic', 'cycled', 'cycles', 'cyclic', 'cygnet', 'cymbal', 'cynics', 'cyprus', 'cystic', 'czechs', 'dabbed', 'dabble', 'dacron', 'dactyl', 'dafter', 'dagger', 'dahlia', 'dainty', 'daises', 'dakota', 'dallas', 'damage', 'damask', 'dammed', 'damned', 'damped', 'damper', 'dampen', 'damsel', 'damson', 'danced', 'dancer', 'dances', 'dander', 'dandle', 'dangle', 'danger', 'danish', 'danker', 'dapper', 'dapple', 'daring', 'darker', 'darkly', 'darken', 'darned', 'darnel', 'darted', 'darter', 'dartle', 'dashed', 'dasher', 'dashes', 'dating', 'dative', 'daubed', 'dauber', 'daunts', 'davits', 'dawdle', 'dawned', 'dayton', 'dazing', 'dazzle', 'deacon', 'deader', 'deadly', 'deaden', 'deafer', 'deafen', 'dealer', 'dearer', 'dearly', 'dearth', 'deaths', 'debars', 'debark', 'debase', 'debate', 'debits', 'debris', 'debtor', 'debugs', 'debunk', 'debuts', 'decade', 'decals', 'decamp', 'decant', 'decays', 'deceit', 'decent', 'decide', 'decked', 'deckle', 'decoct', 'decode', 'decors', 'decoys', 'decree', 'deduce', 'deduct', 'deeded', 'deemed', 'deeper', 'deeply', 'deepen', 'deface', 'defame', 'defeat', 'defect', 'defend', 'defers', 'defied', 'defies', 'defile', 'define', 'deform', 'defray', 'defter', 'deftly', 'defuse', 'degree', 'deiced', 'deicer', 'deices', 'deigns', 'deisms', 'deists', 'deject', 'delays', 'delete', 'delfts', 'deltas', 'delude', 'deluge', 'deluxe', 'delved', 'delves', 'demand', 'demean', 'demise', 'demons', 'demote', 'demurs', 'demure', 'dengue', 'denied', 'denier', 'denies', 'denial', 'denims', 'denote', 'denser', 'dented', 'dental', 'denude', 'denver', 'depart', 'depend', 'depict', 'deploy', 'deport', 'depose', 'depots', 'depths', 'depute', 'deputy', 'derail', 'deride', 'derive', 'dermal', 'dermas', 'desalt', 'descry', 'desert', 'design', 'desire', 'desist', 'despot', 'detach', 'detail', 'detain', 'detect', 'deters', 'detest', 'detour', 'deuced', 'deuces', 'device', 'devils', 'devise', 'devoid', 'devote', 'devour', 'devout', 'dewier', 'dewing', 'dewlap', 'diadem', 'dialed', 'dialer', 'dialog', 'diaper', 'diatom', 'dibble', 'dicers', 'dicing', 'dicker', 'dickey', 'dictum', 'diddle', 'dieing', 'diesel', 'dieted', 'differ', 'digest', 'digger', 'digits', 'diking', 'dilate', 'dilute', 'dimity', 'dimmed', 'dimmer', 'dimple', 'dimwit', 'diners', 'dinged', 'dinghy', 'dining', 'dinned', 'dinner', 'dinted', 'diodes', 'dipole', 'dipped', 'dipper', 'direct', 'direst', 'dirges', 'dirndl', 'disarm', 'disbar', 'disbud', 'discos', 'discus', 'dished', 'dishes', 'dismal', 'dismay', 'disown', 'dispel', 'distal', 'disuse', 'dither', 'dittos', 'divans', 'divers', 'divert', 'divest', 'divide', 'divine', 'diving', 'divots', 'dobbin', 'docent', 'docile', 'docked', 'docket', 'doctor', 'dodder', 'dodged', 'dodger', 'dodges', 'dodoes', 'doffed', 'dogged', 'dogies', 'dogmas', 'doings', 'doling', 'dolled', 'dollar', 'dollop', 'dolman', 'dolmen', 'dolors', 'domain', 'doming', 'domino', 'donate', 'donjon', 'donkey', 'donned', 'donors', 'donuts', 'doodle', 'doodad', 'doomed', 'dopier', 'dopily', 'doping', 'dories', 'dormer', 'dorsal', 'dosage', 'dosing', 'dotage', 'dotard', 'doting', 'dotted', 'dottle', 'doubly', 'double', 'doubts', 'douche', 'doughs', 'doughy', 'dourer', 'dourly', 'doused', 'douses', 'dowels', 'dowers', 'downed', 'downer', 'dowsed', 'dowser', 'dowses', 'dozens', 'dozing', 'drafts', 'drafty', 'dragon', 'drains', 'drakes', 'dramas', 'draped', 'drapes', 'drawer', 'drawls', 'dreads', 'dreams', 'dreamy', 'dreamt', 'dreary', 'dredge', 'drench', 'dressy', 'driest', 'drifts', 'drills', 'drinks', 'drippy', 'driver', 'drives', 'driven', 'drivel', 'drogue', 'droned', 'drones', 'drools', 'droops', 'droopy', 'dropsy', 'drover', 'droves', 'drowns', 'drowse', 'drowsy', 'drudge', 'druids', 'drunks', 'drupes', 'dryads', 'dryers', 'drying', 'dubbed', 'dublin', 'ducats', 'ducked', 'dueled', 'dueler', 'duffer', 'duffel', 'dugong', 'dugout', 'dulcet', 'dulled', 'duller', 'dumber', 'dumdum', 'dumped', 'dunces', 'dunked', 'dunned', 'dupery', 'duping', 'duplex', 'duress', 'during', 'durums', 'dusted', 'duster', 'dustup', 'duties', 'dwarfs', 'dwells', 'dyadic', 'dyeing', 'dynamo', 'dynast', 'eagles', 'eaglet', 'earful', 'earlap', 'earned', 'earner', 'earths', 'earthy', 'earwax', 'earwig', 'easels', 'easier', 'easily', 'easing', 'easter', 'eaters', 'eatery', 'eating', 'ebbing', 'echoed', 'echoes', 'echoic', 'eclair', 'eclats', 'eczema', 'eddied', 'eddies', 'edemas', 'edgier', 'edging', 'edible', 'edicts', 'edited', 'editor', 'educed', 'educes', 'eerier', 'eerily', 'efface', 'effect', 'effete', 'effigy', 'efflux', 'effort', 'effuse', 'egging', 'eggnog', 'egoism', 'egoist', 'egress', 'egrets', 'eiders', 'eights', 'eighty', 'eighth', 'either', 'ejects', 'elands', 'elapse', 'elates', 'elated', 'elbows', 'elders', 'eldest', 'elects', 'eleven', 'elfish', 'elicit', 'elided', 'elides', 'elites', 'elixir', 'eloped', 'elopes', 'eluded', 'eluder', 'eludes', 'embalm', 'embank', 'embark', 'embeds', 'embers', 'emblem', 'embody', 'emboli', 'emboss', 'embryo', 'emcees', 'emceed', 'emends', 'emerge', 'emetic', 'emigre', 'emoted', 'emotes', 'empire', 'employ', 'emptor', 'enable', 'enacts', 'enamel', 'enamor', 'encamp', 'encase', 'encode', 'encore', 'endear', 'ending', 'endive', 'endows', 'endued', 'endues', 'endure', 'enemas', 'energy', 'enfold', 'engage', 'engine', 'engird', 'engirt', 'engulf', 'enigma', 'enjoin', 'enjoys', 'enlace', 'enlist', 'enmesh', 'enmity', 'ennuis', 'enough', 'enrage', 'enrich', 'enroll', 'ensile', 'ensign', 'ensued', 'ensues', 'ensure', 'entail', 'enters', 'entice', 'entire', 'entity', 'entomb', 'entrap', 'entree', 'envied', 'envies', 'envois', 'envoys', 'enwrap', 'enzyme', 'eocene', 'eolith', 'eosins', 'epochs', 'epodes', 'eponym', 'equals', 'equate', 'equine', 'equips', 'equity', 'erased', 'eraser', 'erases', 'erbium', 'erects', 'ergots', 'ermine', 'eroded', 'erodes', 'erotic', 'errand', 'errant', 'errata', 'erring', 'errors', 'ersatz', 'eructs', 'erupts', 'escape', 'eschew', 'escort', 'escrow', 'eskers', 'eskimo', 'espied', 'espies', 'esprit', 'essays', 'estate', 'esteem', 'esters', 'estrus', 'etched', 'etches', 'ethane', 'ethers', 'ethics', 'ethnic', 'ethyls', 'etudes', 'euchre', 'eulogy', 'eunuch', 'eureka', 'europe', 'evaded', 'evades', 'evened', 'evenly', 'events', 'evicts', 'evince', 'evoked', 'evokes', 'evolve', 'exacts', 'exalts', 'exceed', 'excels', 'except', 'excess', 'excise', 'excite', 'excuse', 'exempt', 'exerts', 'exeunt', 'exhale', 'exhort', 'exhume', 'exiled', 'exiles', 'exists', 'exited', 'exodus', 'exotic', 'expand', 'expect', 'expels', 'expend', 'expert', 'expire', 'export', 'expose', 'extant', 'extend', 'extent', 'extols', 'extort', 'extras', 'exuded', 'exudes', 'exults', 'eyecup', 'eyeful', 'eyeing', 'eyelet', 'eyelid', 'fables', 'fabric', 'facade', 'facets', 'facial', 'facile', 'facing', 'factor', 'fading', 'faerie', 'fagged', 'fagots', 'failed', 'faille', 'faints', 'fairer', 'fairly', 'faiths', 'fakers', 'fakery', 'faking', 'fakirs', 'falcon', 'fallen', 'fallow', 'falser', 'falter', 'family', 'famine', 'famous', 'fanged', 'fanned', 'farads', 'farces', 'farina', 'faring', 'farmed', 'farmer', 'farrow', 'fasces', 'fascia', 'fasted', 'faster', 'fasten', 'fatale', 'father', 'fathom', 'fating', 'fatted', 'fatter', 'fatten', 'faucet', 'faults', 'faulty', 'faunas', 'favors', 'fawned', 'fazing', 'fealty', 'feared', 'feasts', 'fecund', 'fedora', 'feebly', 'feeble', 'feeder', 'feeing', 'feeler', 'feigns', 'feints', 'feisty', 'feline', 'felled', 'feller', 'fellow', 'felons', 'felony', 'felted', 'female', 'femmes', 'femurs', 'fenced', 'fencer', 'fences', 'fended', 'fender', 'fennel', 'ferric', 'ferret', 'ferris', 'ferule', 'fervor', 'fervid', 'fester', 'festal', 'feting', 'fetish', 'fetter', 'fettle', 'feuded', 'feudal', 'fevers', 'fewest', 'fezzes', 'fiance', 'fiasco', 'fibbed', 'fibber', 'fibers', 'fibril', 'fibrin', 'fibula', 'fickle', 'fiddle', 'fidget', 'fields', 'fiends', 'fierce', 'fiesta', 'fifing', 'fifths', 'figged', 'fights', 'figure', 'fijian', 'filets', 'filial', 'filing', 'filled', 'filler', 'fillet', 'fillip', 'filmed', 'filter', 'filthy', 'finals', 'finale', 'finder', 'finely', 'finers', 'finery', 'finest', 'finger', 'finial', 'fining', 'finish', 'finite', 'finked', 'finned', 'finnan', 'firing', 'firkin', 'firmed', 'firmer', 'firmly', 'firsts', 'firths', 'fiscal', 'fished', 'fishes', 'fisher', 'fisted', 'fitful', 'fitted', 'fitter', 'fixate', 'fixing', 'fizzed', 'fizzes', 'fizzle', 'fjords', 'flabby', 'flacks', 'flacon', 'flagon', 'flails', 'flairs', 'flaked', 'flakes', 'flamed', 'flames', 'flambe', 'flange', 'flanks', 'flared', 'flares', 'flashy', 'flasks', 'flatly', 'flaunt', 'flavor', 'flawed', 'flaxen', 'flayed', 'flecks', 'fledge', 'fleece', 'fleecy', 'fleets', 'fleshy', 'flexed', 'flexes', 'flexor', 'flicks', 'fliers', 'flight', 'flimsy', 'flinch', 'flings', 'flints', 'flinty', 'flirts', 'flirty', 'floats', 'flocks', 'floods', 'floors', 'floozy', 'floppy', 'floral', 'floras', 'floret', 'florid', 'florin', 'flossy', 'flours', 'floury', 'flouts', 'flowed', 'flower', 'fluent', 'fluffs', 'fluffy', 'fluids', 'fluked', 'flukes', 'flumed', 'flumes', 'flunks', 'flunky', 'flurry', 'fluted', 'flutes', 'fluxed', 'fluxes', 'flybys', 'flying', 'foaled', 'foamed', 'fobbed', 'fodder', 'fogged', 'fogies', 'foible', 'foiled', 'foists', 'folded', 'folder', 'folios', 'folium', 'folksy', 'follow', 'foment', 'fonder', 'fondly', 'fondle', 'fondue', 'fooled', 'footed', 'forage', 'forays', 'forbid', 'forced', 'forces', 'forded', 'forego', 'forest', 'forged', 'forges', 'forger', 'forget', 'forgot', 'forked', 'formed', 'formal', 'format', 'former', 'formic', 'fortes', 'forums', 'fossil', 'foster', 'fought', 'fouled', 'fouler', 'foully', 'founds', 'founts', 'fourth', 'foveae', 'fowled', 'foxier', 'foxily', 'foxing', 'foyers', 'framed', 'framer', 'frames', 'francs', 'france', 'franca', 'franks', 'frappe', 'frauds', 'frayed', 'freaks', 'freaky', 'freely', 'freest', 'freeze', 'french', 'frenzy', 'fresco', 'friars', 'friday', 'friend', 'frieze', 'fright', 'frigid', 'frijol', 'frills', 'frilly', 'fringe', 'frisks', 'frisky', 'frizzy', 'frocks', 'froggy', 'frolic', 'fronds', 'fronts', 'frosts', 'frosty', 'froths', 'frothy', 'frowns', 'frowzy', 'frozen', 'frugal', 'fruits', 'fruity', 'frumps', 'frumpy', 'fryers', 'frying', 'fuddle', 'fudged', 'fudges', 'fueled', 'fugues', 'fuhrer', 'fulled', 'fuller', 'fumble', 'fuming', 'funded', 'fungal', 'fungus', 'funned', 'funnel', 'furies', 'furled', 'furors', 'furred', 'furrow', 'furzes', 'fusees', 'fusing', 'fusion', 'fussed', 'fusses', 'futile', 'future', 'fuzzed', 'fuzzes', 'gabbed', 'gabber', 'gabble', 'gabled', 'gables', 'gadded', 'gadfly', 'gadget', 'gaelic', 'gaffed', 'gaffes', 'gagged', 'gaggle', 'gagmen', 'gagman', 'gaiety', 'gained', 'gainer', 'gaited', 'gaiter', 'galaxy', 'galena', 'galled', 'gallic', 'galley', 'gallon', 'gallop', 'galore', 'galosh', 'gamble', 'gambia', 'gambit', 'gambol', 'gamely', 'gamest', 'gamete', 'gamier', 'gamins', 'gaming', 'gammas', 'gammon', 'gamuts', 'gander', 'ganged', 'gangly', 'gangue', 'gannet', 'gantry', 'gaping', 'gapped', 'garage', 'garbed', 'garble', 'garden', 'gargle', 'garish', 'garlic', 'garner', 'garnet', 'garret', 'garter', 'gashed', 'gashes', 'gasket', 'gasped', 'gassed', 'gasser', 'gasses', 'gather', 'gauche', 'gauged', 'gauges', 'gauzes', 'gavels', 'gavial', 'gawked', 'gayest', 'gayety', 'gazebo', 'gazing', 'geared', 'geckos', 'geezer', 'geiger', 'geisha', 'gelded', 'gemini', 'gemmed', 'gender', 'genera', 'genets', 'geneva', 'genies', 'genial', 'genius', 'genres', 'gently', 'gentle', 'gentry', 'geodes', 'gerbil', 'german', 'gerund', 'getter', 'getups', 'gewgaw', 'geyser', 'ghetto', 'ghosts', 'ghouls', 'giants', 'gibber', 'gibbet', 'gibbon', 'gibing', 'giblet', 'gifted', 'gigged', 'giggly', 'giggle', 'gigolo', 'gigots', 'gigues', 'gilded', 'gilled', 'gimlet', 'gimped', 'ginger', 'ginkgo', 'ginned', 'girded', 'girder', 'girdle', 'girted', 'girths', 'gismos', 'givens', 'givers', 'giving', 'glaces', 'glades', 'gladly', 'glance', 'glands', 'glared', 'glares', 'glassy', 'glazed', 'glazes', 'gleams', 'gleans', 'glibly', 'glided', 'glider', 'glides', 'glints', 'glitch', 'gloats', 'globed', 'globes', 'global', 'glooms', 'gloomy', 'glossy', 'gloved', 'gloves', 'glowed', 'glower', 'glozed', 'glozes', 'gluing', 'glumly', 'glutei', 'gluten', 'glycol', 'glyphs', 'gnarls', 'gnawed', 'gneiss', 'gnomes', 'gnomic', 'gnomon', 'goaded', 'goalie', 'goatee', 'gobble', 'goblet', 'goblin', 'godson', 'gofers', 'goggle', 'goings', 'goiter', 'golder', 'golden', 'golfed', 'golfer', 'gonads', 'goners', 'gonged', 'goober', 'goodly', 'goofed', 'googol', 'gooier', 'gooney', 'goosed', 'gooses', 'gopher', 'gorged', 'gorges', 'gorier', 'goring', 'gorses', 'gospel', 'gossip', 'gothic', 'gotten', 'gouged', 'gouges', 'gourds', 'govern', 'gowned', 'grabby', 'graced', 'graces', 'graded', 'grader', 'grades', 'grafts', 'graham', 'grails', 'grains', 'grainy', 'grands', 'grange', 'granny', 'grants', 'grapes', 'graphs', 'grasps', 'grassy', 'grated', 'grater', 'grates', 'gratis', 'graved', 'graver', 'graven', 'graves', 'gravel', 'gravid', 'grayed', 'grayer', 'grazed', 'grazes', 'grease', 'greasy', 'greats', 'grebes', 'greece', 'greeds', 'greedy', 'greeks', 'greens', 'greets', 'griefs', 'grieve', 'grills', 'grille', 'grimed', 'grimes', 'grimly', 'grinds', 'gringo', 'griped', 'gripes', 'grippe', 'grisly', 'grists', 'gritty', 'groans', 'groats', 'grocer', 'groggy', 'groins', 'grooms', 'groove', 'groovy', 'groped', 'gropes', 'grotto', 'grouch', 'ground', 'groups', 'grouse', 'grouts', 'groves', 'grovel', 'grower', 'growls', 'growth', 'grubby', 'grudge', 'gruels', 'grumps', 'grumpy', 'grunts', 'guanos', 'guards', 'guavas', 'guests', 'guffaw', 'guiana', 'guided', 'guides', 'guidon', 'guiles', 'guilds', 'guilts', 'guilty', 'guinea', 'guises', 'guitar', 'gulfed', 'gulled', 'gullet', 'gulped', 'gumbos', 'gummed', 'gunmen', 'gunman', 'gunned', 'gunner', 'gurgle', 'gushed', 'gusher', 'gushes', 'gusset', 'gusted', 'gustos', 'gutted', 'gutter', 'guyana', 'guying', 'guzzle', 'gypped', 'gypsum', 'gyrate', 'habits', 'hacked', 'hacker', 'hackle', 'hackie', 'haddie', 'hafted', 'haggle', 'haggis', 'hailed', 'hailer', 'hairdo', 'halest', 'halide', 'haling', 'halite', 'halloo', 'hallow', 'halted', 'halter', 'halved', 'halves', 'hamlet', 'hammed', 'hammer', 'hamper', 'handed', 'handle', 'hanged', 'hanger', 'hangar', 'hanker', 'hansom', 'happen', 'harass', 'harbor', 'harder', 'hardly', 'harden', 'harems', 'harked', 'harlot', 'harmed', 'harped', 'harrow', 'hashed', 'hashes', 'hasped', 'hassle', 'hasted', 'hastes', 'hasten', 'hatbox', 'hating', 'hatred', 'hauled', 'haunch', 'haunts', 'havana', 'havens', 'having', 'havocs', 'hawaii', 'hawing', 'hawked', 'hawker', 'hawser', 'hawses', 'haying', 'haymow', 'hazard', 'hazels', 'hazier', 'hazily', 'hazing', 'headed', 'header', 'healed', 'healer', 'health', 'heaped', 'hearer', 'hearse', 'hearts', 'hearty', 'hearth', 'heater', 'heated', 'heaths', 'heaved', 'heaves', 'heaven', 'hebrew', 'heckle', 'hectic', 'hector', 'hedged', 'hedges', 'heeded', 'heehaw', 'heeled', 'heifer', 'height', 'heists', 'helena', 'helium', 'helmet', 'helots', 'helped', 'helper', 'hemmed', 'hennas', 'herald', 'herbed', 'herbal', 'herded', 'hereby', 'herein', 'hereof', 'hereon', 'heresy', 'hereto', 'hermit', 'hernia', 'heroes', 'heroic', 'heroin', 'herons', 'herpes', 'hewing', 'hexane', 'hexing', 'heyday', 'hiatus', 'hiccup', 'hidden', 'hiding', 'hieing', 'higher', 'highly', 'hijack', 'hikers', 'hiking', 'hinder', 'hindus', 'hinged', 'hinges', 'hinted', 'hipper', 'hippie', 'hippos', 'hirers', 'hiring', 'hissed', 'hisses', 'hither', 'hitter', 'hiving', 'hoagie', 'hoards', 'hoarse', 'hoaxed', 'hoaxes', 'hobble', 'hobbit', 'hobnob', 'hoboes', 'hocked', 'hockey', 'hoeing', 'hogged', 'hoists', 'hokums', 'holder', 'holdup', 'holier', 'holing', 'holism', 'holler', 'hollow', 'homage', 'hombre', 'homely', 'homers', 'homier', 'homily', 'hominy', 'homing', 'honcho', 'honest', 'honeys', 'honing', 'honked', 'honker', 'honors', 'hooded', 'hooeys', 'hoofed', 'hoofer', 'hooked', 'hooker', 'hookah', 'hookup', 'hooped', 'hoopla', 'hoopoe', 'hooray', 'hooted', 'hooves', 'hoping', 'hopped', 'hopper', 'hordes', 'horned', 'hornet', 'horror', 'horrid', 'horsed', 'horses', 'hosing', 'hosted', 'hostel', 'hotbed', 'hotels', 'hotter', 'hounds', 'hourly', 'housed', 'houses', 'hovels', 'hovers', 'howdah', 'howled', 'howler', 'hoyden', 'hubbub', 'hubcap', 'hubris', 'huddle', 'hueing', 'huffed', 'hugest', 'hugged', 'hugger', 'hulked', 'hulled', 'humans', 'humane', 'humbly', 'humble', 'humbug', 'hummed', 'humors', 'humped', 'humphs', 'humger', 'hungry', 'hunker', 'hunted', 'hunter', 'hurdle', 'hurled', 'hurler', 'hurrah', 'hurtle', 'hushed', 'hushes', 'husked', 'husker', 'hussar', 'hustle', 'hybrid', 'hyenas', 'hymens', 'hymned', 'hymnal', 'hyphen', 'hyping', 'hyssop', 'iambic', 'ibexes', 'ibises', 'icebox', 'icicle', 'iciest', 'icings', 'ideals', 'ideate', 'idiocy', 'idioms', 'idiots', 'idlers', 'idling', 'idylls', 'igloos', 'ignite', 'ignore', 'iguana', 'imaged', 'images', 'imbibe', 'imbrue', 'imbued', 'imbues', 'immune', 'immure', 'impact', 'impair', 'impale', 'impala', 'impart', 'impede', 'impels', 'impend', 'impish', 'import', 'impose', 'impost', 'impugn', 'impure', 'impute', 'inarch', 'inborn', 'inbred', 'incest', 'inched', 'inches', 'incise', 'incite', 'income', 'incurs', 'indeed', 'indent', 'indies', 'indian', 'indict', 'indigo', 'indite', 'indium', 'indoor', 'induce', 'induct', 'infamy', 'infant', 'infect', 'infers', 'infest', 'infirm', 'inflow', 'influx', 'inform', 'infuse', 'ingest', 'ingots', 'inhale', 'inhere', 'inhume', 'inject', 'injure', 'injury', 'inkier', 'inking', 'inlaid', 'inland', 'inlays', 'inlets', 'inmate', 'inmost', 'innate', 'inning', 'inputs', 'inroad', 'inrush', 'insane', 'insect', 'insert', 'insets', 'inside', 'insist', 'insole', 'instep', 'insult', 'insure', 'intact', 'intake', 'intend', 'intent', 'inters', 'intern', 'intone', 'intuit', 'inulin', 'inured', 'inures', 'invade', 'invent', 'invert', 'invest', 'invite', 'invoke', 'inward', 'iodide', 'iodine', 'iodize', 'ionize', 'ipecac', 'iraqis', 'ireful', 'irises', 'irking', 'ironed', 'ironic', 'irrupt', 'ischia', 'island', 'islets', 'isobar', 'isomer', 'israel', 'issued', 'issues', 'italic', 'itched', 'itches', 'itself', 'jabbed', 'jabber', 'jabots', 'jacked', 'jackal', 'jacket', 'jading', 'jagged', 'jaguar', 'jailed', 'jailer', 'jalopy', 'jammed', 'jangle', 'japans', 'japers', 'japery', 'japing', 'jarful', 'jargon', 'jarred', 'jasper', 'jaunts', 'jaunty', 'jayvee', 'jazzed', 'jazzes', 'jeered', 'jejune', 'jejuna', 'jelled', 'jennet', 'jerboa', 'jerked', 'jerkin', 'jersey', 'jested', 'jester', 'jesuit', 'jetsam', 'jetted', 'jewels', 'jewish', 'jibbed', 'jibing', 'jigged', 'jigger', 'jiggle', 'jigsaw', 'jilted', 'jingle', 'jinxed', 'jinxes', 'jitney', 'jiving', 'jobbed', 'jobber', 'jockey', 'jocose', 'jocund', 'jogged', 'jogger', 'joggle', 'joined', 'joiner', 'joints', 'joists', 'jokers', 'joking', 'jolted', 'jordan', 'joshed', 'joshes', 'jostle', 'jotted', 'joules', 'jounce', 'jousts', 'jovial', 'joyful', 'joyous', 'judaic', 'judged', 'judges', 'juggle', 'juiced', 'juicer', 'juices', 'jujube', 'juleps', 'julian', 'jumble', 'jumbos', 'jumped', 'jumper', 'juncos', 'juneau', 'jungle', 'junior', 'junked', 'junket', 'junkie', 'juntas', 'juries', 'jurist', 'jurors', 'juster', 'justly', 'jutted', 'kabobs', 'kabuki', 'kaiser', 'kansas', 'kaolin', 'kapoks', 'kappas', 'karats', 'karate', 'karmic', 'karmas', 'kashas', 'kayaks', 'kazoos', 'kebabs', 'keeled', 'keened', 'keener', 'keenly', 'keeper', 'kelvin', 'kenned', 'kennel', 'kenyan', 'kernel', 'kettle', 'keycap', 'keying', 'keypad', 'khakis', 'kibitz', 'kibosh', 'kicked', 'kicker', 'kidded', 'kidnap', 'kidney', 'killed', 'killer', 'kilter', 'kimono', 'kinder', 'kindly', 'kindle', 'kingly', 'kinked', 'kiosks', 'kipper', 'kippur', 'kirsch', 'kismet', 'kissed', 'kisser', 'kisses', 'kiting', 'kitsch', 'kitten', 'kliegs', 'kludge', 'knacks', 'knaves', 'kneads', 'kneels', 'knells', 'knifed', 'knifes', 'knight', 'knives', 'knobby', 'knocks', 'knolls', 'knotty', 'knurls', 'koalas', 'kodiak', 'kopeck', 'korean', 'kosher', 'kowtow', 'kraals', 'krills', 'kummel', 'kuwait', 'labels', 'labial', 'labium', 'labors', 'lacers', 'lacier', 'lacing', 'lacked', 'lackey', 'lactic', 'lacuna', 'ladder', 'ladies', 'lading', 'ladled', 'ladles', 'lagers', 'lagged', 'lagoon', 'lairds', 'lamaze', 'lambda', 'lamely', 'lament', 'lamest', 'lamina', 'laming', 'lanais', 'lanced', 'lancer', 'lances', 'lancet', 'landed', 'landau', 'lanker', 'lapels', 'lapins', 'lapped', 'lapper', 'lappet', 'lapsed', 'lapser', 'lapses', 'larded', 'larder', 'larger', 'largos', 'lariat', 'larked', 'larrup', 'larvae', 'larval', 'larynx', 'lasers', 'lashed', 'lashes', 'lasses', 'lassie', 'lassos', 'lasted', 'lastly', 'lastex', 'lately', 'lateen', 'latent', 'latest', 'lathed', 'lathes', 'lather', 'latish', 'latter', 'latvia', 'lauded', 'laughs', 'launch', 'laurel', 'laving', 'lavish', 'lawful', 'lawyer', 'laxest', 'laxity', 'layers', 'laying', 'laymen', 'layman', 'layoff', 'layout', 'lazier', 'lazily', 'lazing', 'lazuli', 'leaded', 'leader', 'leaden', 'leafed', 'league', 'leaked', 'leaned', 'leaner', 'leaped', 'learns', 'learnt', 'leased', 'leases', 'leaves', 'leaven', 'lecher', 'lector', 'ledger', 'ledges', 'leered', 'leeway', 'legacy', 'legate', 'legato', 'legend', 'legged', 'legion', 'legume', 'lemmas', 'lemons', 'lemony', 'lemurs', 'lender', 'length', 'lenity', 'lenses', 'lenten', 'lentil', 'lentos', 'lepers', 'lepton', 'lesion', 'lesser', 'lessor', 'lessee', 'lessen', 'lesson', 'lethal', 'letter', 'letups', 'levant', 'levees', 'levels', 'levers', 'levied', 'levies', 'levity', 'lewder', 'lewdly', 'liable', 'lianas', 'libber', 'libels', 'libido', 'libyan', 'lichen', 'licked', 'lidded', 'lieder', 'lieges', 'lifted', 'lifter', 'lights', 'lignin', 'lignum', 'likely', 'likens', 'liking', 'lilacs', 'lilies', 'lilted', 'limber', 'limbos', 'limits', 'limned', 'limner', 'limped', 'limply', 'limpet', 'limpid', 'linden', 'lineal', 'linear', 'linens', 'liners', 'linger', 'lingos', 'lingua', 'lining', 'linked', 'linker', 'linnet', 'lintel', 'lipase', 'lipids', 'lipoid', 'liquor', 'liquid', 'lisbon', 'lisles', 'lisped', 'listed', 'listen', 'litany', 'litchi', 'liters', 'lither', 'litmus', 'litter', 'little', 'lively', 'livens', 'livers', 'livery', 'living', 'lizard', 'llamas', 'llanos', 'loaded', 'loader', 'loafed', 'loafer', 'loathe', 'loaves', 'lobbed', 'locals', 'locale', 'locate', 'locked', 'locker', 'locket', 'lockup', 'locust', 'lodged', 'lodger', 'lodges', 'lofted', 'logged', 'logger', 'loggia', 'logier', 'logjam', 'loiter', 'lolled', 'london', 'lonely', 'loners', 'longed', 'longer', 'looked', 'lookup', 'loomed', 'looped', 'loosed', 'looser', 'looses', 'loosen', 'looted', 'looter', 'loping', 'lopped', 'lopper', 'lorans', 'lorded', 'lordly', 'losers', 'losing', 'losses', 'lotion', 'lottos', 'louder', 'loudly', 'lounge', 'loupes', 'loused', 'louses', 'louver', 'lovely', 'lovers', 'loving', 'lowboy', 'lowers', 'lowest', 'lowing', 'lubber', 'lucent', 'lucite', 'lucked', 'lugged', 'lulled', 'lumber', 'lumbar', 'lumens', 'lummox', 'lumped', 'lunacy', 'lunged', 'lunges', 'lupine', 'luring', 'lurked', 'lusher', 'lushes', 'lusted', 'luster', 'luxury', 'lyceum', 'lyrics', 'lysins', 'lysine', 'macaws', 'macros', 'macron', 'macula', 'madcap', 'madder', 'madden', 'madmen', 'madman', 'madras', 'madrid', 'maenad', 'maggot', 'magics', 'magnet', 'magnum', 'magpie', 'maguey', 'magyar', 'mahout', 'maiden', 'mailed', 'mailer', 'maimed', 'mainly', 'maizes', 'majors', 'makers', 'making', 'malady', 'malawi', 'malays', 'malaya', 'malice', 'malign', 'mallei', 'mallet', 'mallow', 'malted', 'mambas', 'mambos', 'mammal', 'manage', 'manful', 'manger', 'mangle', 'manias', 'maniac', 'manics', 'manila', 'manioc', 'manned', 'mannas', 'manner', 'manors', 'manque', 'manses', 'mantic', 'mantas', 'mantle', 'mantel', 'mantis', 'mantra', 'manual', 'manure', 'maoist', 'maples', 'mapped', 'maraca', 'maraud', 'marble', 'margin', 'marine', 'marina', 'marino', 'marker', 'marked', 'market', 'markup', 'marlin', 'marmot', 'maroon', 'marred', 'marrow', 'marshy', 'marten', 'martin', 'martyr', 'marvel', 'mascot', 'masers', 'mashed', 'masher', 'mashes', 'mashie', 'masked', 'masons', 'masque', 'massed', 'masses', 'massif', 'master', 'mastic', 'mating', 'matins', 'matrix', 'matron', 'matted', 'matter', 'mattes', 'mature', 'matzos', 'mauled', 'maundy', 'mauves', 'maxims', 'maybes', 'mayday', 'mayfly', 'mayhem', 'mayors', 'meadow', 'meager', 'meaner', 'measly', 'meccas', 'mecums', 'medals', 'meddle', 'medial', 'medias', 'median', 'medics', 'medico', 'medium', 'medley', 'medusa', 'meeker', 'meekly', 'meeter', 'megrim', 'melded', 'melees', 'mellow', 'melody', 'melons', 'melted', 'member', 'memoir', 'memory', 'menace', 'menage', 'mended', 'menial', 'meninx', 'menses', 'mensal', 'mentor', 'mental', 'menthe', 'meowed', 'mercer', 'merely', 'merest', 'merged', 'merger', 'merges', 'merino', 'merits', 'merles', 'merlin', 'mermen', 'merman', 'mescal', 'meshed', 'meshes', 'mesons', 'messed', 'messes', 'metals', 'meteor', 'meters', 'method', 'methyl', 'metier', 'meting', 'metric', 'metros', 'mettle', 'mewing', 'mewled', 'mexico', 'mezzos', 'miasma', 'micros', 'micron', 'midair', 'middle', 'midday', 'midges', 'midget', 'midrib', 'midway', 'miffed', 'mights', 'mighty', 'mignon', 'mikado', 'milady', 'milder', 'mildly', 'mildew', 'milers', 'milieu', 'milked', 'milled', 'miller', 'millet', 'milord', 'mimics', 'miming', 'mimosa', 'minced', 'mincer', 'minces', 'minded', 'miners', 'mingle', 'minims', 'mining', 'minion', 'minnow', 'minors', 'minted', 'minuet', 'minute', 'minxes', 'mirage', 'miring', 'mirror', 'mirths', 'miscue', 'misers', 'misery', 'misfit', 'mishap', 'mislay', 'misled', 'missed', 'misses', 'missal', 'misted', 'mister', 'misuse', 'miters', 'mitten', 'mixers', 'mixing', 'mizzen', 'mobbed', 'mobile', 'mobius', 'mochas', 'mocked', 'mockup', 'models', 'modems', 'modern', 'modest', 'modify', 'modish', 'module', 'moguls', 'mohair', 'moiety', 'moiled', 'moires', 'molars', 'molded', 'molder', 'molest', 'molted', 'molten', 'moment', 'monaco', 'monads', 'monday', 'moneys', 'monger', 'mongol', 'monism', 'monist', 'monkey', 'monody', 'months', 'mooned', 'moored', 'mooted', 'moping', 'mopped', 'moppet', 'morals', 'morale', 'morass', 'morays', 'morbid', 'morels', 'morgue', 'mormon', 'morons', 'morose', 'morrow', 'morsel', 'mortal', 'mortar', 'mosaic', 'moscow', 'moseys', 'moslem', 'mosque', 'mosses', 'mostly', 'motels', 'motets', 'mother', 'motifs', 'motile', 'motion', 'motive', 'motley', 'motors', 'mottle', 'mounds', 'mounts', 'mourns', 'moused', 'mouser', 'mouses', 'mousse', 'mouths', 'mouton', 'movers', 'movies', 'moving', 'mowers', 'mowing', 'mucked', 'mucous', 'muddle', 'muffed', 'muffle', 'muffin', 'muftis', 'mugged', 'mugger', 'mukluk', 'mulcts', 'mulish', 'mulled', 'mullet', 'mumble', 'mummer', 'muppet', 'murals', 'murder', 'murmur', 'muscle', 'muscat', 'museum', 'mushed', 'mushes', 'musics', 'musing', 'muskeg', 'musket', 'muslim', 'muslin', 'mussed', 'musses', 'mussel', 'muster', 'mutant', 'mutate', 'mutely', 'mutest', 'mutiny', 'muting', 'mutter', 'mutton', 'mutual', 'muumuu', 'muzhik', 'muzzle', 'myelin', 'myopia', 'myopic', 'myriad', 'myrrhs', 'myrtle', 'myself', 'mystic', 'nabbed', 'nabobs', 'nacres', 'nadirs', 'nagged', 'nagger', 'naiads', 'nailed', 'naives', 'namely', 'naming', 'napalm', 'napery', 'napkin', 'napped', 'narrow', 'nassau', 'nation', 'native', 'nature', 'naught', 'nausea', 'navels', 'navies', 'nazism', 'neared', 'nearer', 'nearly', 'nearby', 'neater', 'neatly', 'nebula', 'necked', 'nectar', 'needed', 'needle', 'negate', 'neighs', 'nekton', 'nelson', 'nephew', 'nerved', 'nerves', 'nested', 'nestle', 'nether', 'netted', 'nettle', 'neural', 'neuron', 'neuter', 'nevada', 'nevoid', 'newark', 'newels', 'newest', 'newton', 'niacin', 'nibble', 'nicely', 'nicest', 'nicety', 'niched', 'niches', 'nicked', 'nickel', 'nieces', 'nights', 'nimbly', 'nimble', 'nimbus', 'ninety', 'ninths', 'nipped', 'nipper', 'nipple', 'niters', 'nitric', 'nitwit', 'nixing', 'nobler', 'nobles', 'nobody', 'nodded', 'nodder', 'nodule', 'noggin', 'noised', 'noises', 'nomads', 'nonage', 'noncom', 'noodle', 'noosed', 'nooses', 'nordic', 'normal', 'norths', 'norway', 'noshed', 'noshes', 'nosier', 'nosily', 'nosing', 'notary', 'notice', 'notify', 'noting', 'notion', 'nougat', 'novels', 'novena', 'novice', 'nowise', 'nozzle', 'nuance', 'nubble', 'nubbin', 'nubile', 'nuclei', 'nudest', 'nudged', 'nudges', 'nudism', 'nudist', 'nudity', 'nugget', 'numbed', 'number', 'numbly', 'nuncio', 'nursed', 'nurses', 'nutmeg', 'nutria', 'nuzzle', 'nylons', 'nymphs', 'oakums', 'obeyed', 'object', 'oblast', 'oblate', 'oblige', 'oblong', 'oboist', 'obsess', 'obtain', 'obtuse', 'obvert', 'occult', 'occupy', 'occurs', 'oceans', 'ocelot', 'ochers', 'octane', 'octant', 'octave', 'octavo', 'octets', 'ocular', 'oddest', 'oddity', 'odious', 'odiums', 'offend', 'offers', 'office', 'offing', 'offish', 'offset', 'ogling', 'ogress', 'oilier', 'oiling', 'okayed', 'oldest', 'olefin', 'olives', 'omegas', 'omelet', 'onions', 'onrush', 'onsets', 'onuses', 'onward', 'onyxes', 'oodles', 'oolong', 'oomphs', 'oozier', 'oozing', 'opaque', 'opened', 'opener', 'openly', 'operas', 'opiate', 'opined', 'opines', 'opiums', 'oppose', 'optics', 'optima', 'opting', 'option', 'opuses', 'oracle', 'orally', 'orange', 'orated', 'orates', 'orator', 'orbing', 'orbits', 'orchid', 'ordain', 'ordeal', 'orders', 'ordure', 'oregon', 'organs', 'orgasm', 'orgies', 'orient', 'origin', 'oriole', 'orison', 'ormolu', 'ornate', 'ornery', 'orphan', 'oscars', 'oscine', 'osiers', 'osmium', 'osprey', 'ossify', 'others', 'otiose', 'ottawa', 'otters', 'ouches', 'ounces', 'ousted', 'ouster', 'outage', 'outbid', 'outcry', 'outdid', 'outfit', 'outfox', 'outing', 'outlaw', 'outlay', 'outlet', 'output', 'outran', 'outrun', 'outset', 'outwit', 'ouzels', 'overdo', 'overly', 'ovines', 'ovoids', 'ovules', 'ovular', 'owlets', 'owlish', 'owners', 'owning', 'oxalic', 'oxalis', 'oxbows', 'oxeyes', 'oxford', 'oxides', 'oxlips', 'oxtail', 'oxygen', 'oyster', 'ozones', 'pacers', 'pacify', 'pacing', 'packed', 'packer', 'packet', 'padded', 'paddle', 'padres', 'paeans', 'paella', 'pagans', 'paging', 'pagoda', 'pained', 'paints', 'paired', 'palace', 'palate', 'palely', 'palest', 'paling', 'palled', 'pallor', 'pallet', 'pallid', 'palmed', 'palter', 'paltry', 'pamper', 'pampas', 'panama', 'pander', 'pandas', 'panels', 'panics', 'panned', 'panted', 'pantry', 'panzer', 'papacy', 'papaws', 'papaya', 'papers', 'papist', 'papuan', 'parade', 'parcel', 'pardon', 'parent', 'pariah', 'paring', 'parish', 'parity', 'parked', 'parkas', 'parlor', 'parlay', 'parley', 'parody', 'parole', 'parrot', 'parsed', 'parses', 'parsec', 'parson', 'parted', 'partly', 'pascal', 'pashas', 'passed', 'passes', 'passim', 'pasted', 'pastes', 'pastas', 'pastel', 'pastor', 'pastry', 'patchy', 'patens', 'patent', 'pathos', 'patina', 'patios', 'patois', 'patrol', 'patron', 'patted', 'patter', 'paunch', 'pauper', 'paused', 'pauses', 'pavans', 'paving', 'pawing', 'pawned', 'payday', 'payees', 'paying', 'payoff', 'payola', 'peachy', 'peahen', 'peaked', 'pealed', 'peanut', 'pearly', 'pearls', 'peavey', 'pebbly', 'pebble', 'pecans', 'pecked', 'pectic', 'pectin', 'pedals', 'pedant', 'peddle', 'peeked', 'peeled', 'peeler', 'peeped', 'peered', 'peeved', 'peeves', 'peewee', 'pegged', 'peking', 'pekoes', 'pelage', 'pellet', 'pelted', 'pelvic', 'pelvis', 'pencil', 'penmen', 'penman', 'penned', 'pennon', 'pentad', 'penult', 'penury', 'people', 'peplum', 'pepped', 'pepper', 'pepsin', 'peptic', 'perils', 'period', 'perish', 'perked', 'permit', 'persia', 'person', 'perter', 'peruke', 'peruse', 'peseta', 'pester', 'pestle', 'petals', 'petard', 'peters', 'petite', 'petrel', 'petrol', 'petted', 'pewees', 'pewits', 'pewter', 'peyote', 'phalli', 'phased', 'phases', 'phenol', 'phenyl', 'phlegm', 'phloem', 'phobia', 'phobic', 'phoebe', 'phoned', 'phones', 'phonic', 'phonon', 'phooey', 'photos', 'photon', 'phrase', 'phylum', 'physic', 'pianos', 'piazza', 'picked', 'pickle', 'pickax', 'picket', 'pickup', 'picnic', 'picots', 'picric', 'piddle', 'pidgin', 'pieced', 'pieces', 'pierce', 'pierre', 'piffle', 'pigeon', 'pigged', 'piglet', 'pignut', 'pigpen', 'pigsty', 'pikers', 'pilafs', 'pileup', 'pilfer', 'piling', 'pilled', 'pillar', 'pillow', 'pilots', 'pimped', 'pimply', 'pimple', 'pinata', 'pincer', 'pineal', 'pinged', 'pinier', 'pining', 'pinion', 'pinked', 'pinker', 'pinkly', 'pinkie', 'pinned', 'pinons', 'pintos', 'pinups', 'pinyin', 'pipers', 'piping', 'pipits', 'pipkin', 'pipped', 'pippin', 'piqued', 'piques', 'piquet', 'piracy', 'pirate', 'pisces', 'pistil', 'pistol', 'piston', 'pitied', 'pities', 'pitons', 'pitted', 'pivots', 'pixels', 'pixies', 'pizzas', 'placer', 'places', 'placid', 'plague', 'plaice', 'plaids', 'plains', 'plaint', 'plaits', 'planed', 'planer', 'planes', 'planar', 'planet', 'planks', 'plants', 'plaque', 'plasma', 'plated', 'plates', 'platen', 'platys', 'played', 'player', 'playas', 'plazas', 'pleads', 'please', 'pleats', 'plebes', 'pledge', 'plenty', 'pleura', 'plexus', 'pliant', 'pliers', 'plight', 'plinth', 'plisse', 'plover', 'plowed', 'plucks', 'plucky', 'plumed', 'plumes', 'plumbs', 'plumps', 'plunge', 'plunks', 'plural', 'pluses', 'plying', 'pocket', 'podium', 'poetic', 'poetry', 'pogrom', 'points', 'pointy', 'poised', 'poises', 'poison', 'pokers', 'pokeys', 'pokier', 'pokily', 'poking', 'poland', 'poleax', 'police', 'policy', 'poling', 'polios', 'polish', 'polity', 'polite', 'polkas', 'polled', 'pollen', 'polyps', 'pomade', 'pommel', 'pompom', 'pompon', 'poncho', 'ponder', 'pongee', 'ponies', 'pontes', 'poodle', 'pooled', 'pooped', 'poorer', 'poorly', 'popery', 'popgun', 'popish', 'poplar', 'poplin', 'popped', 'popper', 'poring', 'porker', 'porous', 'porter', 'portly', 'portal', 'posers', 'poseur', 'posies', 'posing', 'posits', 'posses', 'possum', 'posted', 'postal', 'poster', 'potash', 'potato', 'potent', 'pother', 'potion', 'potpie', 'potted', 'potter', 'pounce', 'pounds', 'poured', 'pouted', 'pouter', 'powder', 'powers', 'powwow', 'prague', 'praise', 'prance', 'pranks', 'prated', 'prates', 'prawns', 'praxes', 'praxis', 'prayed', 'prayer', 'preach', 'precis', 'preens', 'prefab', 'prefer', 'prefix', 'premed', 'premix', 'prepay', 'presto', 'pretty', 'prewar', 'preyed', 'priced', 'prices', 'pricks', 'prided', 'prides', 'priers', 'priest', 'primed', 'primer', 'primes', 'primly', 'primal', 'primps', 'prince', 'prints', 'priors', 'priory', 'priori', 'prisms', 'prison', 'prissy', 'privet', 'prized', 'prizes', 'probed', 'probes', 'proems', 'profit', 'prolix', 'prompt', 'prongs', 'pronto', 'proofs', 'propel', 'proper', 'prosed', 'proses', 'proton', 'proved', 'proves', 'proven', 'prowls', 'prudes', 'pruned', 'prunes', 'prying', 'psalms', 'pseudo', 'psyche', 'psycho', 'public', 'pucker', 'puddle', 'pueblo', 'puerto', 'puffed', 'puffer', 'puffin', 'puking', 'puling', 'pulled', 'pullet', 'pulley', 'pulped', 'pulpit', 'pulsed', 'pulses', 'pulsar', 'pumice', 'pummel', 'pumped', 'punchy', 'pundit', 'punier', 'punish', 'punned', 'punted', 'punter', 'pupate', 'pupils', 'puppet', 'pureed', 'purees', 'purely', 'purest', 'purged', 'purges', 'purify', 'purism', 'purist', 'purity', 'purled', 'purple', 'purred', 'pursed', 'purser', 'purses', 'pursue', 'purvey', 'pushed', 'pusher', 'pushes', 'pushup', 'pusses', 'putoff', 'putout', 'putrid', 'putsch', 'putted', 'putter', 'puttee', 'puzzle', 'pylons', 'pyrite', 'python', 'quacks', 'quaffs', 'quahog', 'quails', 'quaint', 'quaked', 'quaker', 'quakes', 'qualms', 'quanta', 'quarks', 'quarry', 'quarts', 'quarto', 'quartz', 'quasar', 'quaver', 'queasy', 'quebec', 'queens', 'queers', 'quells', 'quench', 'querns', 'quests', 'queued', 'queues', 'quiche', 'quicks', 'quiets', 'quills', 'quilts', 'quince', 'quinsy', 'quints', 'quires', 'quirks', 'quirky', 'quirts', 'quiver', 'quoins', 'quoits', 'quorum', 'quoted', 'quotes', 'quotas', 'rabble', 'rabbet', 'rabbis', 'rabbit', 'rabies', 'raceme', 'racers', 'racier', 'racial', 'racing', 'racism', 'racist', 'racked', 'racket', 'radars', 'radial', 'radian', 'radios', 'radish', 'radium', 'radius', 'radons', 'raffle', 'raffia', 'rafted', 'rafter', 'ragged', 'raging', 'raglan', 'ragout', 'ragtag', 'raided', 'raider', 'railed', 'rained', 'raised', 'raiser', 'raises', 'raisin', 'rajahs', 'raking', 'rakish', 'ramble', 'ramies', 'ramify', 'ramjet', 'rammed', 'ramose', 'ramous', 'ramrod', 'rancho', 'rancid', 'rancor', 'random', 'ranged', 'ranger', 'ranges', 'ranked', 'ranker', 'rankle', 'ransom', 'ranted', 'rapier', 'rapids', 'rapine', 'raping', 'rapist', 'rapped', 'rappel', 'rarely', 'rarefy', 'rarest', 'raring', 'rarity', 'rascal', 'rasher', 'rashes', 'rashly', 'rasped', 'rather', 'ratify', 'rating', 'ratios', 'ration', 'ratite', 'ratted', 'ratter', 'rattle', 'rattan', 'ravage', 'ravels', 'ravens', 'ravine', 'raving', 'ravish', 'rawest', 'rayons', 'razing', 'razors', 'razzed', 'razzes', 'reacts', 'reader', 'really', 'realms', 'realty', 'reamed', 'reamer', 'reaped', 'reaper', 'reared', 'rearms', 'reason', 'rebate', 'rebels', 'rebind', 'reboot', 'reborn', 'rebuff', 'rebuke', 'rebuts', 'recall', 'recant', 'recaps', 'recast', 'recede', 'recent', 'recess', 'recipe', 'recite', 'reckon', 'recoil', 'record', 'recoup', 'rectal', 'rectos', 'rector', 'rectum', 'recurs', 'redact', 'redcap', 'redder', 'redden', 'redeem', 'redoes', 'redone', 'reduce', 'reefed', 'reefer', 'reeked', 'reeled', 'reeves', 'refers', 'refill', 'refine', 'refits', 'reflex', 'reform', 'refuel', 'refuge', 'refund', 'refuse', 'refute', 'regain', 'regale', 'regard', 'regent', 'reggae', 'regime', 'region', 'regret', 'rehash', 'rehear', 'reigns', 'reined', 'reject', 'rejoin', 'relate', 'relays', 'relent', 'relied', 'relies', 'relics', 'relief', 'reline', 'relish', 'relive', 'remade', 'remain', 'remake', 'remand', 'remark', 'remedy', 'remind', 'remiss', 'remits', 'remora', 'remote', 'remove', 'rename', 'render', 'renege', 'renews', 'rennet', 'rennin', 'renown', 'rented', 'rental', 'reopen', 'repaid', 'repair', 'repast', 'repays', 'repeal', 'repeat', 'repels', 'repent', 'repine', 'replay', 'report', 'repose', 'repute', 'reread', 'reruns', 'resale', 'rescue', 'resell', 'resent', 'resets', 'reship', 'reside', 'resign', 'resins', 'resist', 'resold', 'resort', 'rested', 'result', 'resume', 'retail', 'retain', 'retake', 'retard', 'retell', 'retina', 'retire', 'retold', 'retook', 'retool', 'retort', 'return', 'retype', 'revamp', 'reveal', 'revels', 'revere', 'revert', 'review', 'revile', 'revise', 'revive', 'revoir', 'revoke', 'revolt', 'revues', 'revved', 'reward', 'rewind', 'rewire', 'reword', 'rework', 'rezone', 'rhesus', 'rheums', 'rheumy', 'rhinos', 'rhymed', 'rhymes', 'rhythm', 'rialto', 'riatas', 'ribald', 'ribbed', 'ribbon', 'ribose', 'ricans', 'ricers', 'richer', 'riches', 'richly', 'ricing', 'ridded', 'ridden', 'riddle', 'riders', 'ridged', 'ridges', 'riding', 'rifest', 'riffle', 'rifled', 'rifles', 'rifted', 'rigged', 'rigger', 'rights', 'rigors', 'riling', 'riming', 'rimmed', 'ringed', 'ringer', 'rinsed', 'rinses', 'rioted', 'rioter', 'ripens', 'ripest', 'ripped', 'ripper', 'ripple', 'ripsaw', 'risers', 'rising', 'risked', 'risque', 'ritual', 'rivals', 'rivers', 'rivets', 'riving', 'riyadh', 'roamed', 'roared', 'roasts', 'robbed', 'robber', 'robins', 'robing', 'robots', 'robust', 'rocked', 'rocker', 'rocket', 'rococo', 'rodent', 'rodeos', 'rogues', 'roiled', 'rolled', 'roller', 'romans', 'romany', 'romeos', 'romped', 'romper', 'rondos', 'roofed', 'roofer', 'rookie', 'roomed', 'roomer', 'roosts', 'rooted', 'ropier', 'roping', 'rosary', 'rosier', 'rosins', 'roster', 'rotary', 'rotate', 'rotors', 'rotted', 'rotten', 'rotund', 'rouged', 'rouges', 'roughs', 'rounds', 'roused', 'rouses', 'rousts', 'routed', 'routes', 'rovers', 'roving', 'rowans', 'rowels', 'rowing', 'rubbed', 'rubble', 'rubber', 'rubies', 'rubles', 'rubric', 'ruched', 'ruches', 'ruckus', 'rudder', 'rudely', 'rudest', 'rueful', 'ruffed', 'ruffle', 'rugged', 'ruined', 'rulers', 'ruling', 'rumble', 'rumbas', 'rumors', 'rumple', 'rumpus', 'runlet', 'runner', 'runnel', 'runway', 'rushed', 'rusher', 'rushes', 'russet', 'russia', 'rusted', 'rustle', 'rustic', 'rutted', 'rwanda', 'sabers', 'sables', 'sabots', 'sachem', 'sacher', 'sachet', 'sacked', 'sacred', 'sacrum', 'sadder', 'saddle', 'sadden', 'sadism', 'sadist', 'safari', 'safely', 'safest', 'safety', 'sagely', 'sagest', 'sagged', 'sahibs', 'sailed', 'sailor', 'saints', 'salaam', 'salads', 'salami', 'salary', 'saline', 'saliva', 'sallow', 'salmon', 'salons', 'saloon', 'salted', 'saluki', 'salute', 'salved', 'salver', 'salves', 'salvia', 'salvos', 'samara', 'sambas', 'samoan', 'sampan', 'sample', 'sancta', 'sanded', 'sander', 'sandal', 'sanest', 'sanity', 'sansei', 'santas', 'sapped', 'sarong', 'sashed', 'sashes', 'sashay', 'sassed', 'sasses', 'sateen', 'satins', 'satiny', 'sating', 'satire', 'satrap', 'saturn', 'satyrs', 'sauced', 'saucer', 'sauces', 'saunas', 'sautes', 'sauted', 'savage', 'savant', 'savers', 'savior', 'saving', 'savors', 'savory', 'sawing', 'sawyer', 'saxons', 'saying', 'scabby', 'scaled', 'scales', 'scalar', 'scalds', 'scalps', 'scamps', 'scampi', 'scants', 'scanty', 'scared', 'scares', 'scarab', 'scarce', 'scarfs', 'scarps', 'scathe', 'scenes', 'scenic', 'scents', 'scheme', 'schema', 'schism', 'schist', 'schlep', 'school', 'schuss', 'schwas', 'scions', 'sclera', 'scoffs', 'scolds', 'scones', 'sconce', 'scoops', 'scoots', 'scopes', 'scored', 'scores', 'scorch', 'scoria', 'scorns', 'scoter', 'scotch', 'scours', 'scouts', 'scowls', 'scrams', 'scraps', 'scrape', 'scrawl', 'scream', 'screed', 'screen', 'screws', 'screwy', 'scribe', 'scrims', 'scrimp', 'scrips', 'script', 'scrods', 'scroll', 'scrota', 'scrubs', 'scruff', 'scubas', 'scuffs', 'sculls', 'sculpt', 'scurfy', 'scurry', 'scurvy', 'scythe', 'seabee', 'sealed', 'sealer', 'seamed', 'seamen', 'seaman', 'seance', 'seared', 'search', 'season', 'seated', 'seaway', 'secant', 'secede', 'second', 'secret', 'sector', 'secure', 'sedans', 'sedate', 'seders', 'sedges', 'seduce', 'sedums', 'seeded', 'seeing', 'seeker', 'seemed', 'seemly', 'seeped', 'seesaw', 'seethe', 'seined', 'seines', 'seized', 'seizes', 'seldom', 'select', 'seller', 'selves', 'semite', 'senate', 'sender', 'senile', 'senior', 'sennas', 'sensed', 'senses', 'sensor', 'sentry', 'sepals', 'sepias', 'sepses', 'sepsis', 'septic', 'septet', 'septum', 'sequel', 'sequin', 'serape', 'seraph', 'serene', 'serest', 'serges', 'series', 'serial', 'serifs', 'sermon', 'serums', 'served', 'server', 'serves', 'sesame', 'sestet', 'setoff', 'setter', 'settee', 'settle', 'setups', 'sevens', 'severs', 'severe', 'sevres', 'sewage', 'sewers', 'sewing', 'sexier', 'sexily', 'sexing', 'sexism', 'sexist', 'sextet', 'sexton', 'sexual', 'shabby', 'shacks', 'shaded', 'shades', 'shadow', 'shafts', 'shaggy', 'shaker', 'shakes', 'shaken', 'shakos', 'shales', 'shalom', 'shamed', 'shames', 'shaman', 'shamus', 'shanks', 'shanty', 'shaped', 'shapes', 'shared', 'shares', 'shards', 'sharks', 'sharps', 'shasta', 'shaved', 'shaver', 'shaves', 'shaven', 'shawls', 'shears', 'sheath', 'sheave', 'sheers', 'sheets', 'sheiks', 'shekel', 'shells', 'shelve', 'sherry', 'shield', 'shiers', 'shiest', 'shifts', 'shifty', 'shills', 'shimmy', 'shined', 'shiner', 'shines', 'shinny', 'shinto', 'shires', 'shirks', 'shirrs', 'shirts', 'shiver', 'shoals', 'shoats', 'shocks', 'shoddy', 'shoers', 'shofar', 'shogun', 'shooed', 'shoots', 'shored', 'shores', 'shoran', 'shorts', 'should', 'shouts', 'shoved', 'shoves', 'shovel', 'showed', 'shower', 'shrank', 'shreds', 'shrews', 'shrewd', 'shriek', 'shrift', 'shrike', 'shrill', 'shrimp', 'shrine', 'shrink', 'shrive', 'shroud', 'shrove', 'shrubs', 'shrugs', 'shrunk', 'shtick', 'shucks', 'shunts', 'shying', 'sibyls', 'sicily', 'sicker', 'sickly', 'sickle', 'sicken', 'siding', 'sidled', 'sidles', 'sieged', 'sieges', 'sienna', 'sierra', 'siesta', 'sieved', 'sieves', 'sifted', 'sifter', 'sighed', 'sights', 'sigmas', 'signed', 'signer', 'signal', 'signet', 'silage', 'silent', 'silica', 'silked', 'silken', 'silted', 'silver', 'simian', 'simile', 'simmer', 'simony', 'simper', 'simply', 'simple', 'sinews', 'sinewy', 'sinful', 'singed', 'singer', 'singes', 'singly', 'single', 'sinker', 'sinned', 'sinner', 'siphon', 'sipped', 'sirens', 'siring', 'sirius', 'sisals', 'sister', 'sitars', 'sitcom', 'siting', 'sitter', 'sixths', 'sizing', 'sizzle', 'skater', 'skates', 'skeets', 'skeins', 'sketch', 'skewed', 'skewer', 'skidoo', 'skiers', 'skiffs', 'skiing', 'skills', 'skimps', 'skimpy', 'skinks', 'skinny', 'skirls', 'skirts', 'skivvy', 'skulks', 'skulls', 'skunks', 'skying', 'skylab', 'skyway', 'slacks', 'slaked', 'slakes', 'slalom', 'slangs', 'slangy', 'slants', 'slated', 'slates', 'slaved', 'slaves', 'slavic', 'slaver', 'slayer', 'sleazy', 'sledge', 'sleeks', 'sleeps', 'sleepy', 'sleets', 'sleeve', 'sleigh', 'sleuth', 'sliced', 'slices', 'slicks', 'slided', 'slider', 'slides', 'sliest', 'slight', 'slimed', 'slimes', 'slimly', 'slings', 'slinks', 'slinky', 'sliver', 'slogan', 'sloops', 'sloped', 'slopes', 'sloppy', 'slouch', 'slough', 'slovak', 'sloven', 'slowed', 'slower', 'slowly', 'sludge', 'sludgy', 'sluice', 'sluing', 'slumps', 'slurps', 'slurry', 'slushy', 'smacks', 'smalls', 'smarmy', 'smarts', 'smears', 'smeary', 'smells', 'smelly', 'smelts', 'smiled', 'smiles', 'smilax', 'smirch', 'smirks', 'smites', 'smiths', 'smithy', 'smocks', 'smoggy', 'smoked', 'smoker', 'smokes', 'smooch', 'smooth', 'smudge', 'smudgy', 'smugly', 'smutty', 'snacks', 'snafus', 'snails', 'snaked', 'snakes', 'snappy', 'snared', 'snares', 'snarls', 'snatch', 'snazzy', 'sneaks', 'sneaky', 'sneers', 'sneeze', 'snider', 'sniffs', 'sniped', 'sniper', 'snipes', 'snippy', 'snitch', 'snivel', 'snobby', 'snoods', 'snoops', 'snoopy', 'snooty', 'snooze', 'snored', 'snores', 'snorts', 'snotty', 'snouts', 'snowed', 'snuffs', 'snugly', 'soaked', 'soaped', 'soared', 'soaves', 'sobbed', 'sobers', 'soccer', 'social', 'socked', 'socket', 'sodded', 'sodden', 'sodium', 'sodomy', 'softer', 'softly', 'soften', 'soigne', 'soiled', 'soiree', 'solace', 'solder', 'solely', 'solemn', 'solids', 'soling', 'soloed', 'solute', 'solved', 'solves', 'somali', 'somber', 'sonata', 'sonnet', 'sooner', 'sooted', 'soothe', 'sopped', 'sordid', 'sorely', 'sorest', 'sorrel', 'sorrow', 'sorted', 'sorter', 'sortie', 'soughs', 'sought', 'sounds', 'soured', 'sourer', 'sourly', 'source', 'soused', 'souses', 'soviet', 'sowing', 'spaced', 'spacer', 'spaces', 'spaded', 'spades', 'spadix', 'spanks', 'spared', 'sparer', 'spares', 'sparks', 'sparse', 'spasms', 'spates', 'spathe', 'spavin', 'spawns', 'spayed', 'speaks', 'spears', 'specie', 'specks', 'speech', 'speeds', 'speedy', 'spells', 'spends', 'sperms', 'spewed', 'sphere', 'sphinx', 'spiced', 'spices', 'spider', 'spiels', 'spiffy', 'spigot', 'spiked', 'spikes', 'spills', 'spines', 'spinal', 'spinet', 'spires', 'spiral', 'spirit', 'spited', 'spites', 'splash', 'splays', 'spleen', 'splice', 'splint', 'splits', 'spodes', 'spoils', 'spoily', 'spoked', 'spokes', 'spoken', 'sponge', 'spongy', 'spoofs', 'spooks', 'spooky', 'spools', 'spoons', 'spoors', 'spores', 'sports', 'sporty', 'spotty', 'spouse', 'spouts', 'sprain', 'sprang', 'sprats', 'sprawl', 'sprays', 'sprees', 'spread', 'sprier', 'sprigs', 'spring', 'sprint', 'sprits', 'sprite', 'sprout', 'spruce', 'sprung', 'spumed', 'spumes', 'spunks', 'spunky', 'spurge', 'spurns', 'spurts', 'sputum', 'spying', 'squabs', 'squads', 'squall', 'square', 'squash', 'squats', 'squaws', 'squawk', 'squeak', 'squeal', 'squibs', 'squids', 'squint', 'squire', 'squirm', 'squirt', 'squish', 'stable', 'stacks', 'staffs', 'staged', 'stages', 'stains', 'stairs', 'staked', 'stakes', 'staled', 'staler', 'stales', 'stalks', 'stalls', 'stamen', 'stamps', 'stance', 'stanch', 'stands', 'stanza', 'stapes', 'staple', 'stared', 'stares', 'starch', 'starry', 'starts', 'starve', 'stases', 'stasis', 'stated', 'static', 'stator', 'states', 'statue', 'status', 'staved', 'staves', 'stayed', 'steads', 'steady', 'steaks', 'steals', 'steams', 'steamy', 'steeds', 'steels', 'steely', 'steeps', 'steers', 'steins', 'steles', 'stench', 'stenos', 'steppe', 'stereo', 'sterns', 'sterna', 'sterol', 'stewed', 'sticks', 'sticky', 'stifle', 'stiffs', 'stigma', 'stiles', 'stills', 'stilts', 'stings', 'stingy', 'stinks', 'stinky', 'stints', 'stitch', 'storts', 'stocks', 'stocky', 'stodgy', 'stoics', 'stoked', 'stoker', 'stokes', 'stoles', 'stolen', 'stolid', 'stomps', 'stoned', 'stones', 'stooge', 'stools', 'stoops', 'stored', 'stores', 'storks', 'storms', 'stormy', 'stoups', 'stouts', 'stoves', 'stowed', 'strafe', 'strain', 'strait', 'strand', 'straps', 'strata', 'strati', 'straws', 'strays', 'streak', 'stream', 'street', 'stress', 'strews', 'strewn', 'striae', 'strict', 'stride', 'strife', 'strike', 'string', 'strips', 'stripe', 'stripy', 'strive', 'strobe', 'strode', 'stroke', 'stroll', 'strong', 'strops', 'strove', 'struck', 'strums', 'strung', 'struts', 'stubby', 'stucco', 'studio', 'stuffs', 'stuffy', 'stumps', 'stumpy', 'stunts', 'stupid', 'stupor', 'sturdy', 'styled', 'styles', 'stylus', 'stymie', 'subbed', 'subdue', 'sublet', 'submit', 'suborn', 'subset', 'subtly', 'subtle', 'suburb', 'subway', 'succor', 'sucked', 'sucker', 'suckle', 'sudden', 'suedes', 'suffer', 'suffix', 'sugars', 'sugary', 'suited', 'suites', 'suitor', 'sulfas', 'sulfur', 'sulked', 'sullen', 'sultan', 'sultry', 'sumacs', 'summed', 'summer', 'summit', 'summon', 'sunder', 'sundae', 'sunday', 'sundry', 'sunken', 'sunlit', 'sunned', 'sunset', 'suntan', 'sunups', 'supers', 'superb', 'supine', 'supped', 'supper', 'supple', 'supply', 'surely', 'surest', 'surety', 'surfer', 'surged', 'surges', 'surrey', 'surtax', 'survey', 'sutras', 'suttee', 'suture', 'svelte', 'swaged', 'swages', 'swains', 'swamis', 'swamps', 'swampy', 'swanky', 'swards', 'swarms', 'swatch', 'swaths', 'swathe', 'swayed', 'swears', 'sweats', 'sweaty', 'swedes', 'sweden', 'sweeps', 'sweets', 'swells', 'swerve', 'swifts', 'swills', 'swings', 'swiped', 'swipes', 'swirls', 'switch', 'swivel', 'swoons', 'swoops', 'swords', 'sylphs', 'sylvan', 'symbol', 'syndic', 'synods', 'syntax', 'syrian', 'syrinx', 'syrups', 'syrupy', 'system', 'tabard', 'tabbed', 'tabled', 'tables', 'tablet', 'taboos', 'tabors', 'tacked', 'tackle', 'tactic', 'tagged', 'tahiti', 'taigas', 'tailed', 'tailor', 'taints', 'taipei', 'taiwan', 'takers', 'taking', 'talcum', 'talent', 'talked', 'talker', 'talkie', 'taller', 'tallow', 'talmud', 'talons', 'tamale', 'tamest', 'taming', 'tamped', 'tamper', 'tampon', 'tandem', 'tangle', 'tangos', 'tanker', 'tanned', 'tannic', 'tanner', 'tannin', 'taoism', 'taoist', 'tapers', 'taping', 'tapirs', 'tapped', 'tappet', 'target', 'tariff', 'tarmac', 'tarots', 'tarpon', 'tarred', 'tarsal', 'tarsus', 'tarter', 'tartly', 'tartan', 'tartar', 'tasked', 'tassel', 'tasted', 'taster', 'tastes', 'tatted', 'tatter', 'tattle', 'tattoo', 'taught', 'taunts', 'taupes', 'taurus', 'tauter', 'tautly', 'tavern', 'tawdry', 'taxied', 'taxies', 'taxing', 'teacup', 'teamed', 'teapot', 'teared', 'teased', 'teaser', 'teases', 'teasel', 'tedium', 'teeing', 'teemed', 'teeter', 'teethe', 'teflon', 'teller', 'temple', 'temper', 'tempos', 'tempts', 'tenant', 'tended', 'tender', 'tendon', 'tenets', 'tennis', 'tenons', 'tenors', 'tenpin', 'tensed', 'tenser', 'tenses', 'tensor', 'tented', 'tenter', 'tenths', 'tenure', 'tepees', 'termed', 'terror', 'terser', 'tested', 'tester', 'testes', 'testae', 'testis', 'tether', 'tetras', 'tetrad', 'texans', 'thalia', 'thanes', 'thanks', 'thatch', 'thawed', 'thecae', 'thefts', 'theirs', 'theism', 'theist', 'themes', 'thence', 'theory', 'theses', 'thesis', 'thieve', 'thighs', 'thinly', 'things', 'thinks', 'thirds', 'thirst', 'thirty', 'thongs', 'thorax', 'thorns', 'thorny', 'though', 'thrash', 'threes', 'thread', 'threat', 'thresh', 'thrice', 'thrift', 'thrill', 'thrive', 'throes', 'throat', 'throbs', 'throne', 'throng', 'throve', 'throws', 'thrown', 'thrums', 'thrush', 'thrust', 'thumbs', 'thumps', 'thwack', 'thwart', 'thymes', 'thymic', 'thymus', 'tiaras', 'tibiae', 'tibial', 'ticked', 'ticker', 'tickle', 'ticket', 'tidbit', 'tidied', 'tidier', 'tidies', 'tidily', 'tiding', 'tiered', 'tierce', 'tiffed', 'tigers', 'tights', 'tildes', 'tiling', 'tilled', 'tiller', 'tilted', 'tilths', 'timber', 'timbre', 'timely', 'timers', 'timing', 'tinder', 'tinged', 'tingly', 'tingle', 'tinges', 'tinier', 'tinker', 'tinkle', 'tinned', 'tinsel', 'tinted', 'tipped', 'tipper', 'tipple', 'tippet', 'tiptoe', 'tiptop', 'tirade', 'tiring', 'tisane', 'tissue', 'titans', 'tithed', 'tithes', 'titian', 'titled', 'titles', 'titter', 'tittle', 'toasts', 'tobago', 'tobies', 'tocsin', 'todays', 'toddle', 'toeing', 'toffee', 'togged', 'toggle', 'toiled', 'toiles', 'toilet', 'tokens', 'tolled', 'tomato', 'tomboy', 'tomcat', 'tomcod', 'tomtit', 'tongue', 'tonics', 'toning', 'tonsil', 'tooled', 'tooted', 'toothy', 'topeka', 'topers', 'topics', 'toping', 'topped', 'topper', 'topple', 'toques', 'torahs', 'torero', 'tories', 'torpor', 'torpid', 'torque', 'torrid', 'torsos', 'tortes', 'tossed', 'tosses', 'tossup', 'totals', 'totems', 'toting', 'totted', 'totter', 'toucan', 'touche', 'touchy', 'toughs', 'toupee', 'toured', 'tousle', 'touted', 'touter', 'towage', 'toward', 'towels', 'towers', 'towhee', 'towing', 'townie', 'toxics', 'toxins', 'toying', 'traces', 'tracer', 'tracks', 'tracts', 'traded', 'trader', 'trades', 'tragic', 'trails', 'trains', 'traits', 'tramps', 'trance', 'trashy', 'trauma', 'travel', 'trawls', 'treads', 'treats', 'treaty', 'trebly', 'treble', 'tremor', 'trench', 'trends', 'trendy', 'trepan', 'triads', 'triage', 'trials', 'tribes', 'tribal', 'trices', 'tricks', 'tricky', 'tricot', 'trifle', 'trijet', 'trills', 'trines', 'trinal', 'triode', 'tripes', 'triply', 'triple', 'tripod', 'triter', 'triton', 'triune', 'trivet', 'trivia', 'troche', 'troika', 'trojan', 'trolls', 'tromps', 'troops', 'tropes', 'trophy', 'tropic', 'troths', 'trough', 'troupe', 'trowel', 'truant', 'truces', 'trucks', 'trudge', 'truest', 'truing', 'truism', 'trumps', 'trunks', 'trusts', 'trusty', 'truths', 'trying', 'tryout', 'trysts', 'tubbed', 'tubers', 'tubing', 'tubule', 'tucked', 'tucker', 'tufted', 'tugged', 'tugger', 'tulips', 'tulles', 'tumble', 'tumefy', 'tumors', 'tumuli', 'tumult', 'tundra', 'tuners', 'tunics', 'tuning', 'tunnel', 'tupelo', 'turban', 'turbid', 'turbot', 'tureen', 'turgid', 'turkic', 'turkey', 'turned', 'turner', 'turnip', 'turret', 'turtle', 'tuscan', 'tussle', 'tussah', 'tutors', 'tuxedo', 'twangs', 'tweaks', 'tweeds', 'tweedy', 'twelve', 'twenty', 'twiggy', 'twills', 'twined', 'twines', 'twinge', 'twirls', 'twists', 'twitch', 'tycoon', 'typhus', 'typify', 'typing', 'typist', 'tyrant', 'udders', 'uganda', 'uglier', 'ukases', 'ulcers', 'ulster', 'ultima', 'ultimo', 'ultras', 'umbels', 'umbers', 'umbras', 'umbrae', 'umiaks', 'umlaut', 'umpire', 'unable', 'unbars', 'unbend', 'unbent', 'unbind', 'unbolt', 'unborn', 'uncaps', 'uncial', 'uncles', 'unclad', 'uncoil', 'uncork', 'undies', 'undoes', 'undone', 'unduly', 'unease', 'uneasy', 'uneven', 'unfair', 'unfelt', 'unfits', 'unfold', 'unfurl', 'unglue', 'unhand', 'unholy', 'unhook', 'unhurt', 'unions', 'unique', 'unisex', 'unison', 'united', 'unites', 'univac', 'unjust', 'unkind', 'unlace', 'unless', 'unlike', 'unload', 'unlock', 'unmade', 'unmake', 'unmans', 'unmask', 'unpack', 'unpaid', 'unpins', 'unplug', 'unread', 'unreal', 'unreel', 'unrest', 'unripe', 'unroll', 'unruly', 'unsafe', 'unsaid', 'unseal', 'unseat', 'unseen', 'unshod', 'unsnap', 'unsold', 'unstop', 'unsung', 'unsure', 'untied', 'unties', 'untidy', 'untold', 'untrue', 'unused', 'unveil', 'unwary', 'unwell', 'unwind', 'unwise', 'unworn', 'unwrap', 'unyoke', 'unzips', 'upbeat', 'update', 'upends', 'upheld', 'uphill', 'uphold', 'upkeep', 'upland', 'uplift', 'uppers', 'upping', 'uppity', 'uproar', 'uproot', 'upsets', 'upshot', 'uptake', 'uptown', 'upturn', 'upward', 'upwind', 'uranus', 'urbane', 'urchin', 'uremia', 'ureter', 'urgent', 'urging', 'urines', 'urinal', 'ursine', 'usably', 'usable', 'usages', 'useful', 'ushers', 'usurer', 'usurps', 'uterus', 'utmost', 'utopia', 'utters', 'uvulas', 'uvular', 'vacant', 'vacate', 'vacuum', 'vagary', 'vagina', 'vaguer', 'vainer', 'vainly', 'valets', 'valise', 'valley', 'valors', 'valued', 'values', 'valves', 'vamped', 'vandal', 'vanish', 'vanity', 'vanned', 'vapors', 'varied', 'varies', 'varlet', 'vassal', 'vaster', 'vastly', 'vatted', 'vaults', 'vaunts', 'vector', 'veered', 'veiled', 'veined', 'velars', 'veldts', 'vellum', 'velour', 'velvet', 'vended', 'vendor', 'vendee', 'veneer', 'venial', 'venire', 'venoms', 'venous', 'vented', 'venues', 'verbal', 'verged', 'verger', 'verges', 'verify', 'verily', 'verity', 'vermin', 'vernal', 'versed', 'verses', 'versos', 'versus', 'vertex', 'verves', 'vesper', 'vessel', 'vested', 'vestal', 'vestry', 'vetoed', 'vetoes', 'vetted', 'vexing', 'viable', 'vialed', 'viands', 'vicars', 'victim', 'victor', 'vicuna', 'videos', 'vienna', 'viewed', 'viewer', 'vigils', 'viking', 'vilest', 'vilify', 'villas', 'vinous', 'vinyls', 'violas', 'violet', 'violin', 'vipers', 'virago', 'vireos', 'virgin', 'virile', 'virtue', 'visaed', 'visage', 'viscid', 'vishnu', 'vision', 'visits', 'visors', 'vistas', 'visual', 'vitals', 'vivify', 'vixens', 'vizard', 'vizier', 'vocals', 'vodkas', 'vogues', 'voiced', 'voices', 'voided', 'voiles', 'volant', 'volley', 'volume', 'volute', 'volvox', 'vomits', 'voodoo', 'vortex', 'votary', 'voters', 'voting', 'votive', 'vowels', 'vowing', 'voyage', 'voyeur', 'vulcan', 'vulgar', 'vulvae', 'vyings', 'wadded', 'waddle', 'wading', 'wafers', 'waffle', 'wafted', 'wagers', 'wagged', 'waggle', 'waging', 'wagons', 'wailed', 'waists', 'waited', 'waiter', 'waived', 'waiver', 'waives', 'wakens', 'waking', 'waling', 'walked', 'walker', 'walkup', 'walled', 'wallet', 'wallop', 'wallow', 'walnut', 'walrus', 'wampum', 'wander', 'wangle', 'waning', 'wankel', 'wanner', 'wanted', 'wanton', 'wapiti', 'warble', 'warded', 'warder', 'warden', 'warier', 'warily', 'warmed', 'warmer', 'warmly', 'warmth', 'warned', 'warped', 'warred', 'warren', 'warsaw', 'washed', 'washer', 'washes', 'wasted', 'waster', 'wastes', 'waters', 'watery', 'wattle', 'wavers', 'wavier', 'waving', 'waxier', 'waxing', 'waylay', 'weaker', 'weakly', 'weaken', 'wealth', 'weaned', 'weapon', 'wearer', 'weasel', 'weaver', 'weaves', 'webbed', 'wedded', 'wedged', 'wedges', 'wedgie', 'weeded', 'weekly', 'weenie', 'weevil', 'weighs', 'weight', 'weirdo', 'welded', 'welder', 'welkin', 'welled', 'welted', 'welter', 'wended', 'weskit', 'wetted', 'wetter', 'whacks', 'whaled', 'whaler', 'whales', 'whammy', 'wheals', 'wheats', 'wheels', 'wheeze', 'wheezy', 'whelks', 'whelms', 'whelps', 'whence', 'wherry', 'whiffs', 'whiled', 'whiles', 'whilom', 'whilst', 'whimsy', 'whined', 'whines', 'whinny', 'whirls', 'whisks', 'whists', 'whited', 'whiter', 'whites', 'whiten', 'wholes', 'wholly', 'whoops', 'whoosh', 'whored', 'whores', 'whorls', 'wicker', 'wicked', 'wicket', 'widely', 'widens', 'widest', 'widget', 'widows', 'widths', 'wields', 'wiener', 'wifely', 'wigged', 'wiggle', 'wigwag', 'wigwam', 'wildly', 'wilder', 'wilier', 'wiling', 'willed', 'willow', 'wilted', 'wimple', 'winced', 'winces', 'winded', 'window', 'winery', 'winged', 'wining', 'winked', 'winkle', 'winner', 'winnow', 'winter', 'wintry', 'wipers', 'wiping', 'wirier', 'wiring', 'wisdom', 'wisely', 'wisest', 'wished', 'wishes', 'wising', 'wisped', 'wither', 'withes', 'withal', 'within', 'wizard', 'wizens', 'wobbly', 'wobble', 'woeful', 'wolfed', 'wolves', 'wombat', 'wonder', 'wonted', 'wooded', 'wooden', 'woofed', 'woofer', 'wooing', 'woolly', 'woolen', 'worded', 'worked', 'worker', 'worlds', 'wormed', 'worsen', 'worsts', 'worths', 'worthy', 'wounds', 'wowing', 'wracks', 'wraith', 'wraths', 'wreaks', 'wreath', 'wrecks', 'wrench', 'wrests', 'wretch', 'wriest', 'wrings', 'wrists', 'writer', 'writes', 'writhe', 'wrongs', 'wursts', 'xebecs', 'xmases', 'xylems', 'yachts', 'yahoos', 'yahweh', 'yakked', 'yammer', 'yanked', 'yankee', 'yapped', 'yarrow', 'yawing', 'yawned', 'yearly', 'yearns', 'yeasts', 'yeasty', 'yelled', 'yeller', 'yellow', 'yelped', 'yentas', 'yeomen', 'yeoman', 'yessed', 'yesses', 'yields', 'yipped', 'yippee', 'yodels', 'yogurt', 'yokels', 'yoking', 'yonder', 'yorker', 'youths', 'yttric', 'yuccas', 'zaftig', 'zambia', 'zanier', 'zanies', 'zapped', 'zealot', 'zebras', 'zenith', 'zephyr', 'zeroed', 'zeroes', 'zigzag', 'zinced', 'zinged', 'zinger', 'zinnia', 'zipped', 'zipper', 'zircon', 'zither', 'zodiac', 'zombie', 'zoning', 'zonked', 'zooids', 'zoomed', 'zygoma', 'zygote', 'zymase', 'abalone', 'abandon', 'abasing', 'abasers', 'abashed', 'abashes', 'abating', 'abaters', 'abdomen', 'abducts', 'abetted', 'abettor', 'abeyant', 'abiders', 'abiding', 'ability', 'abjured', 'abjurer', 'abjures', 'abolish', 'aborted', 'abounds', 'abraded', 'abrader', 'abrades', 'abreast', 'abridge', 'abscess', 'abscise', 'abscond', 'absence', 'absents', 'absolve', 'absorbs', 'abstain', 'abusing', 'abusers', 'abusive', 'abutted', 'abutter', 'abysmal', 'abysses', 'abyssal', 'acacias', 'academe', 'academy', 'acceded', 'acceder', 'accedes', 'accents', 'accepts', 'acclaim', 'accords', 'accosts', 'account', 'accrued', 'accrues', 'accrual', 'accused', 'accuser', 'accuses', 'acerbic', 'acetate', 'acetone', 'acetyls', 'achieve', 'acidity', 'acidify', 'acolyte', 'aconite', 'acquire', 'acquits', 'acreage', 'acrilan', 'acrobat', 'acronym', 'acrylic', 'actinic', 'actions', 'actress', 'actuary', 'actuate', 'acutely', 'adagios', 'adamant', 'adapted', 'adapter', 'addaxes', 'addends', 'addenda', 'addicts', 'addling', 'address', 'adduced', 'adducer', 'adduces', 'adducts', 'adenine', 'adenoid', 'adeptly', 'adhered', 'adheres', 'adipose', 'adjoins', 'adjourn', 'adjudge', 'adjunct', 'adjured', 'adjurer', 'adjures', 'adjusts', 'admired', 'admirer', 'admires', 'admiral', 'admixed', 'admixes', 'adopted', 'adopter', 'adorers', 'adoring', 'adorned', 'adrenal', 'adsorbs', 'adulate', 'advance', 'advents', 'adverbs', 'adverse', 'adverts', 'adviser', 'advises', 'advised', 'advisor', 'aegises', 'aerated', 'aerates', 'aerator', 'aerials', 'aerobes', 'aerobic', 'aerosol', 'affably', 'affable', 'affairs', 'affects', 'affirms', 'affixed', 'affixes', 'afflict', 'affords', 'affrays', 'affront', 'afghans', 'african', 'against', 'ageisms', 'ageless', 'agendas', 'agendum', 'aggress', 'agility', 'agilely', 'agitate', 'agonies', 'agonize', 'agoutis', 'aground', 'aileron', 'ailemnt', 'aimless', 'airbags', 'airdrop', 'airflow', 'airfoil', 'airglow', 'airiest', 'airings', 'airless', 'airlift', 'airline', 'airmail', 'airport', 'airship', 'airsick', 'airways', 'alabama', 'aladdin', 'alarmed', 'alaskan', 'albania', 'albedos', 'albinos', 'albumen', 'albumin', 'alcazar', 'alchemy', 'alcohol', 'alcoves', 'alembic', 'alerted', 'alertly', 'alewife', 'alfalfa', 'algebra', 'algeria', 'algiers', 'aliases', 'alibied', 'aliened', 'alights', 'aligned', 'aliment', 'alimony', 'aliquot', 'alkalis', 'allayed', 'alleles', 'alleger', 'alleges', 'alleged', 'allegro', 'allelic', 'allergy', 'allonym', 'allowed', 'alloyed', 'alluded', 'alluder', 'alludes', 'allured', 'allures', 'allying', 'almanac', 'almoner', 'almonds', 'alnicos', 'alpacas', 'already', 'altered', 'althers', 'althorn', 'alumina', 'alumnae', 'alumnus', 'alveoli', 'alyssum', 'amalgam', 'amanita', 'amassed', 'amasses', 'amateur', 'amatory', 'amazing', 'amazons', 'ambient', 'ambling', 'amblers', 'amenity', 'amended', 'amerced', 'amerces', 'america', 'amiably', 'amiable', 'amities', 'ammeter', 'ammonia', 'amnesia', 'amnesty', 'amnions', 'amongst', 'amorous', 'amounts', 'amperes', 'amphora', 'amplest', 'amplify', 'ampoule', 'amputee', 'amulets', 'amusing', 'amylase', 'anagram', 'analogs', 'analogy', 'analyst', 'analyze', 'ananias', 'anapest', 'anaphor', 'anarchy', 'anatomy', 'anchors', 'anchovy', 'ancient', 'andante', 'andiron', 'andorra', 'android', 'anemias', 'anemone', 'aneroid', 'angeles', 'angelic', 'angered', 'anginas', 'angling', 'anglers', 'angoras', 'angrily', 'angrier', 'anguish', 'angular', 'aniline', 'animism', 'animals', 'animate', 'animist', 'anionic', 'aniseed', 'anklets', 'anneals', 'annelid', 'annexed', 'annexes', 'annoyed', 'annuity', 'annuals', 'annular', 'annulus', 'anodize', 'anodyne', 'anoints', 'anomaly', 'another', 'answers', 'antacid', 'anteing', 'antenna', 'anthers', 'anthems', 'anthill', 'anthrax', 'antigen', 'antique', 'antiwar', 'antlers', 'antonio', 'antonym', 'anxiety', 'anxious', 'anybody', 'anymore', 'anytime', 'anywise', 'aoudads', 'aphasic', 'aphasia', 'aphelia', 'apogees', 'apology', 'apostle', 'apothem', 'appalls', 'apparel', 'appeals', 'appears', 'appease', 'appends', 'applies', 'applaud', 'applied', 'applier', 'appoint', 'apposed', 'apposes', 'apprise', 'approve', 'apricot', 'apropos', 'aptness', 'aquatic', 'aqueous', 'aquifer', 'aquiver', 'arables', 'arabian', 'aramaic', 'arbiter', 'arbutus', 'arcades', 'arching', 'archers', 'archaic', 'archery', 'archive', 'archway', 'ardency', 'arduous', 'areaway', 'arguing', 'arguers', 'argyles', 'aridity', 'arising', 'arizona', 'armadas', 'armband', 'armenia', 'armfuls', 'armhole', 'armlets', 'armload', 'armoire', 'armored', 'armpits', 'armrest', 'arnicas', 'aroused', 'arouses', 'arousal', 'arraign', 'arrange', 'arrayed', 'arrears', 'arrests', 'arrived', 'arrives', 'arrival', 'arroyos', 'arsenic', 'arsenal', 'artiest', 'article', 'artisan', 'artists', 'artless', 'artwork', 'ascends', 'ascents', 'ascetic', 'ascribe', 'asepses', 'asepsis', 'aseptic', 'asexual', 'ashamed', 'ashtray', 'asiatic', 'asinine', 'askance', 'asocial', 'aspects', 'asperse', 'asphalt', 'aspired', 'aspires', 'aspirin', 'assagai', 'assails', 'assault', 'assayed', 'assayer', 'assents', 'asserts', 'assigns', 'assists', 'assizes', 'assorts', 'assuage', 'assumed', 'assumes', 'assures', 'assured', 'asthmas', 'astound', 'astride', 'asunder', 'asylums', 'atavism', 'ataxics', 'ataxias', 'atelier', 'atheism', 'atheist', 'athirst', 'athlete', 'athwart', 'atlanta', 'atlases', 'atomize', 'atoning', 'atriums', 'atrophy', 'attache', 'attacks', 'attains', 'attaint', 'attempt', 'attends', 'attests', 'attired', 'attires', 'attract', 'attuned', 'attunes', 'auburns', 'auction', 'audibly', 'audible', 'audited', 'auditor', 'augment', 'augured', 'augusta', 'aurally', 'aureate', 'aureole', 'auricle', 'aurochs', 'auroras', 'austere', 'austral', 'austria', 'authors', 'autisms', 'automat', 'autopsy', 'autumns', 'avarice', 'avatars', 'avenged', 'avenges', 'avenues', 'average', 'averred', 'averted', 'aviator', 'avidity', 'avocado', 'avocets', 'avoided', 'avowing', 'avowals', 'awaited', 'awaking', 'awakens', 'awarded', 'awesome', 'awfully', 'awkward', 'awnings', 'axially', 'axolotl', 'azaleas', 'azimuth', 'azurite', 'babbles', 'babbitt', 'babbled', 'baboons', 'babying', 'babyish', 'babylon', 'bacchus', 'bacilli', 'backing', 'backers', 'backbit', 'backlog', 'backups', 'badgers', 'badness', 'baffles', 'baffled', 'bagging', 'baggier', 'baggage', 'baghdad', 'bagpipe', 'bahamas', 'bahrain', 'bailing', 'bailers', 'bailiff', 'bailout', 'baiting', 'baklava', 'balance', 'balcony', 'balding', 'baldest', 'baldric', 'baleens', 'baleful', 'balking', 'balkier', 'balkans', 'ballads', 'ballast', 'ballets', 'balloon', 'ballots', 'balmier', 'balsams', 'bamboos', 'bananas', 'banding', 'bandies', 'bandage', 'bandbox', 'bandeau', 'bandied', 'bandits', 'baneful', 'banging', 'bangles', 'bangkok', 'banking', 'bankers', 'banning', 'banners', 'banquet', 'banshee', 'banters', 'bantams', 'banyans', 'baobabs', 'baptist', 'baptism', 'baptize', 'barbing', 'barbers', 'barbels', 'barbell', 'barging', 'bargain', 'barites', 'bariums', 'barking', 'barkers', 'barleys', 'barmaid', 'baronet', 'baroque', 'barques', 'barring', 'barrage', 'barrels', 'barrens', 'barrier', 'barrios', 'barroom', 'barrows', 'barters', 'baryons', 'basalts', 'basemen', 'baseman', 'basenji', 'bashing', 'bashful', 'basilar', 'basking', 'baskets', 'basques', 'bassets', 'bassoon', 'basting', 'bastion', 'bastard', 'batboys', 'batched', 'batches', 'bateaux', 'bathing', 'bathers', 'bathtub', 'batiste', 'batsmen', 'batsman', 'batting', 'battier', 'battens', 'batters', 'battery', 'battled', 'battles', 'baubles', 'bauxite', 'bawdily', 'bawdier', 'bawling', 'bayonet', 'bazaars', 'bazooka', 'beached', 'beaches', 'beacons', 'beading', 'beadier', 'beadles', 'beagles', 'beakers', 'beaming', 'beaning', 'beanies', 'beanbag', 'bearing', 'bearers', 'bearded', 'bearish', 'beastly', 'beating', 'beaters', 'beatify', 'beatnik', 'beavers', 'becalms', 'because', 'beckons', 'becloud', 'becomes', 'bedaubs', 'bedbugs', 'bedding', 'bedecks', 'bedevil', 'bedewed', 'bedizen', 'bedouin', 'bedpans', 'bedpost', 'bedrock', 'bedroll', 'bedroom', 'bedside', 'bedsore', 'bedtime', 'beeches', 'beefing', 'beefier', 'beehive', 'beeline', 'beeping', 'beeswax', 'beetles', 'beetled', 'befalls', 'befores', 'befouls', 'begging', 'beggars', 'begirds', 'begonia', 'begrime', 'beguile', 'beguine', 'behaved', 'behaves', 'beheads', 'behests', 'behinds', 'beholds', 'behoove', 'beijing', 'bejewel', 'belabor', 'belated', 'belayed', 'belched', 'belches', 'beldams', 'belgian', 'belgium', 'beliefs', 'believe', 'belling', 'bellies', 'bellboy', 'bellhop', 'bellied', 'bellows', 'belongs', 'beloved', 'belting', 'belugas', 'belying', 'bemired', 'bemires', 'bemoans', 'bemused', 'bemuses', 'benched', 'benches', 'bending', 'benders', 'beneath', 'benefit', 'benelux', 'bengali', 'benison', 'bennies', 'benthic', 'benthos', 'benumbs', 'benzene', 'benzine', 'benzoic', 'benzols', 'bequest', 'berated', 'berates', 'berbers', 'bereave', 'bermuda', 'bernard', 'berries', 'berserk', 'berthed', 'berthas', 'beseech', 'beseems', 'besides', 'besiege', 'besmear', 'bespeak', 'bespoke', 'besting', 'bestial', 'bestirs', 'bestows', 'bestrew', 'betakes', 'betaken', 'bethink', 'betided', 'betides', 'betimes', 'betoken', 'betrays', 'betroth', 'betting', 'bettors', 'betters', 'between', 'betwixt', 'beveled', 'bewails', 'bewared', 'bewares', 'bewitch', 'bezeled', 'bezique', 'biasing', 'biaxial', 'bibelot', 'bickers', 'bicolor', 'bicycle', 'bidding', 'bidders', 'biddies', 'bifocal', 'biggest', 'bighorn', 'bigoted', 'bigotry', 'bigwigs', 'bikeway', 'bikinis', 'bilging', 'bilious', 'bilking', 'billing', 'billies', 'billion', 'billets', 'billows', 'billowy', 'bimetal', 'binding', 'binders', 'bindery', 'biology', 'bionics', 'biopsic', 'biotins', 'biplane', 'bepolar', 'birched', 'birches', 'birdies', 'birdied', 'biretta', 'biscuit', 'bisects', 'bishops', 'bismuth', 'bisques', 'bistros', 'bitched', 'bitches', 'bitters', 'bittern', 'bitumen', 'bivalve', 'bivouac', 'bizarre', 'blabbed', 'blabber', 'blacked', 'blacker', 'blacken', 'bladder', 'blaming', 'blanche', 'blander', 'blandly', 'blanked', 'blanker', 'blankly', 'blanket', 'blaring', 'blarney', 'blasted', 'blatant', 'blather', 'blazing', 'blazers', 'blazons', 'bleaker', 'bleakly', 'bleared', 'bleated', 'bleeder', 'bleeped', 'blemish', 'blended', 'blender', 'blesses', 'blessed', 'blights', 'blinded', 'blinder', 'blindly', 'blinked', 'blinker', 'blipped', 'blisses', 'blister', 'blither', 'blitzed', 'blitzes', 'bloated', 'blocked', 'blondes', 'blooded', 'bloomed', 'bloomer', 'blooper', 'blossom', 'blotchy', 'blotted', 'blotter', 'bloused', 'blouses', 'blowing', 'blowers', 'blowgun', 'blowout', 'blowups', 'blubber', 'blucher', 'bluffed', 'bluffer', 'blunder', 'blunted', 'blunter', 'bluntly', 'blurred', 'blurted', 'blushed', 'blusher', 'blushes', 'bluster', 'boarded', 'boarder', 'boasted', 'boaster', 'boating', 'boaters', 'boatels', 'boatmen', 'boatman', 'bobbing', 'bobbies', 'bobbles', 'bobbins', 'bobbled', 'bobcats', 'bobsled', 'bobtail', 'bodices', 'bodings', 'bodkins', 'bogeyed', 'bogging', 'boggier', 'boggles', 'boggled', 'boguses', 'bohemia', 'boiling', 'boilers', 'boldest', 'boleros', 'bolivia', 'bologna', 'bolster', 'bolting', 'boluses', 'bombing', 'bombers', 'bombard', 'bombast', 'bonanza', 'bonbons', 'bonding', 'bondage', 'bonfire', 'bonging', 'boniest', 'bonnier', 'bonnets', 'bonuses', 'boobies', 'boodles', 'boodled', 'boohoos', 'booking', 'bookers', 'bookies', 'bookend', 'bookish', 'booklet', 'booming', 'boorish', 'boosted', 'booster', 'booting', 'booties', 'bootees', 'bootleg', 'boozing', 'boozier', 'bopping', 'borates', 'boraxes', 'borders', 'boredom', 'borough', 'borrows', 'borscht', 'borzois', 'boscage', 'boskier', 'bosomed', 'bossing', 'bossily', 'bossier', 'bossism', 'botched', 'botches', 'bothers', 'bottles', 'bottled', 'bottoms', 'boudoir', 'boulder', 'bounced', 'bouncer', 'bounces', 'bounded', 'bounder', 'bounden', 'bouquet', 'bourbon', 'bourses', 'bovines', 'bowfins', 'bowling', 'bowlers', 'bowlegs', 'bowline', 'boxcars', 'boxiest', 'boxwood', 'boycott', 'boyhood', 'bracing', 'bracers', 'bracken', 'bracket', 'bragged', 'bragger', 'brahmas', 'brahman', 'brahmin', 'braided', 'braille', 'brained', 'braised', 'braises', 'braking', 'brambly', 'bramble', 'branded', 'brasher', 'brashly', 'brasses', 'braving', 'bravest', 'bravado', 'bravely', 'bravery', 'bravura', 'brawled', 'brawler', 'braying', 'brayers', 'brazing', 'brazier', 'breadth', 'breaker', 'breakup', 'breasts', 'breaths', 'breathe', 'breathy', 'breeder', 'breezed', 'breezes', 'brevity', 'brevets', 'brewing', 'brewers', 'brewery', 'bribing', 'bribery', 'bricked', 'bridals', 'bridles', 'bridged', 'bridges', 'bridled', 'briefed', 'briefer', 'briefly', 'brigade', 'brigand', 'brimful', 'brimmed', 'brinier', 'brindle', 'brioche', 'brisker', 'briskly', 'brisket', 'bristly', 'bristle', 'britain', 'british', 'britons', 'brittle', 'broader', 'broadly', 'broadax', 'broaden', 'brocade', 'brogans', 'brogues', 'broiled', 'broiler', 'brokers', 'bromide', 'bromine', 'bronchi', 'broncos', 'bronzed', 'bronzes', 'brooded', 'brooder', 'brooked', 'brothel', 'brother', 'brought', 'browned', 'brownie', 'browsed', 'browses', 'bruised', 'bruiser', 'bruises', 'bruited', 'brunets', 'brushed', 'brushes', 'brusque', 'brutish', 'bubbles', 'bubbled', 'bubonic', 'bucking', 'buckles', 'buckets', 'buckeye', 'buckled', 'buckler', 'buckram', 'bucksaw', 'bucolic', 'budding', 'budders', 'buddies', 'budging', 'budgies', 'budgets', 'buffing', 'buffers', 'buffalo', 'buffets', 'buffoon', 'bugaboo', 'bugbear', 'bugging', 'buggies', 'bugling', 'buglers', 'builder', 'bulbous', 'bulbars', 'bulbuls', 'bulging', 'bulgier', 'bulimia', 'bulking', 'bulkily', 'bulkier', 'bulling', 'bullies', 'bullion', 'bulldog', 'bullets', 'bullied', 'bullish', 'bullock', 'bullpen', 'bulrush', 'bulwark', 'bumbles', 'bumbled', 'bumming', 'bumping', 'bumpers', 'bumpily', 'bumpier', 'bumpkin', 'bunched', 'bunches', 'bundles', 'bundled', 'bunging', 'bungles', 'bungled', 'bungler', 'bunions', 'bunking', 'bunkers', 'bunkums', 'bunnies', 'bunting', 'buoying', 'buoyant', 'burbles', 'burbled', 'burdens', 'burdock', 'bureaus', 'burette', 'burgers', 'burgles', 'burgeon', 'burgess', 'burgher', 'burgled', 'burglar', 'burials', 'burling', 'burlier', 'burlaps', 'burmese', 'burning', 'burners', 'burnish', 'burnout', 'burping', 'burring', 'burrows', 'bursars', 'burundi', 'burying', 'busboys', 'bushing', 'bushier', 'bushels', 'busiest', 'buskins', 'bussing', 'busting', 'busters', 'bustles', 'bustard', 'bustled', 'busying', 'butanes', 'butcher', 'butlers', 'butting', 'butters', 'buttery', 'buttock', 'buttons', 'butyric', 'buzzing', 'buzzers', 'buzzard', 'bygones', 'byroads', 'bywords', 'cabanas', 'cabaret', 'cabbing', 'cabbies', 'cabbage', 'cabinet', 'cabling', 'caboose', 'caching', 'cachets', 'cackles', 'cackled', 'cadaver', 'caddied', 'caddies', 'caddish', 'cadence', 'cadenza', 'cadging', 'cadmium', 'caducei', 'caesars', 'caesura', 'caftans', 'cagiest', 'cahoots', 'caimans', 'caisson', 'caitiff', 'cajoles', 'cajoled', 'calcify', 'calcine', 'calcite', 'calcium', 'calculi', 'caldron', 'calends', 'caliber', 'caliper', 'caliphs', 'calling', 'callers', 'callous', 'calming', 'calmest', 'calomel', 'caloric', 'calorie', 'calumet', 'calumny', 'calving', 'calvary', 'calypso', 'calyxes', 'camases', 'cambers', 'cambium', 'cambric', 'camelot', 'cameral', 'cameras', 'camping', 'campers', 'camphor', 'canapes', 'canards', 'canasta', 'cancans', 'cancels', 'cancers', 'candors', 'candles', 'candela', 'candied', 'candies', 'candled', 'canines', 'cankers', 'canning', 'cannily', 'cannier', 'cannels', 'cannery', 'cannons', 'canting', 'canters', 'cantors', 'cantles', 'cantata', 'canteen', 'canthus', 'cantina', 'cantons', 'canvass', 'canyons', 'capably', 'capable', 'capered', 'capital', 'capitol', 'capping', 'caprice', 'capsize', 'capstan', 'capsule', 'captors', 'captain', 'caption', 'captive', 'capture', 'caracal', 'caracas', 'carafes', 'caramel', 'caravan', 'caravel', 'caraway', 'carbide', 'carbine', 'carbons', 'carboys', 'carcass', 'carding', 'cardiac', 'careers', 'careens', 'careful', 'carfare', 'cargoes', 'carhops', 'carious', 'caribou', 'caritas', 'carload', 'carmine', 'carnage', 'caroled', 'caroler', 'caromed', 'carotid', 'carouse', 'carping', 'carpels', 'carpets', 'carport', 'carrion', 'carrels', 'carried', 'carrier', 'carries', 'carrots', 'carsick', 'carting', 'cartage', 'cartels', 'cartons', 'cartoon', 'carving', 'carvers', 'casabas', 'cascade', 'cascara', 'caseins', 'cashing', 'cashews', 'cashier', 'casings', 'casinos', 'caskets', 'cassava', 'cassias', 'cassock', 'casting', 'casters', 'castors', 'castles', 'castled', 'castoff', 'casuals', 'casuist', 'catalan', 'catalog', 'catalpa', 'catarrh', 'catbird', 'catboat', 'catcall', 'catcher', 'catches', 'catered', 'caterer', 'catfish', 'catguts', 'cathode', 'cations', 'catkins', 'catlike', 'catnaps', 'catnips', 'cattier', 'cattail', 'catwalk', 'caudate', 'caulked', 'causing', 'caustic', 'caution', 'cavalry', 'caveats', 'caverns', 'caviars', 'caviled', 'caviler', 'cavorts', 'cayenne', 'cayuses', 'ceasing', 'cedilla', 'cedings', 'ceiling', 'celebre', 'celesta', 'celiacs', 'cellist', 'cellars', 'celsius', 'cements', 'censers', 'censors', 'censure', 'centers', 'centaur', 'centavo', 'centime', 'central', 'century', 'ceramic', 'cereals', 'cerises', 'ceriums', 'cermets', 'certain', 'certify', 'cervine', 'cession', 'ceteras', 'chablis', 'chafing', 'chaffed', 'chaffer', 'chagrin', 'chained', 'chaired', 'chaises', 'chalets', 'chalice', 'chalked', 'challis', 'chamber', 'chamfer', 'chamois', 'champed', 'chanced', 'chances', 'chancel', 'chancre', 'changed', 'changes', 'channel', 'chanson', 'chanted', 'chanter', 'chantey', 'chaotic', 'chapeau', 'chapels', 'chaplet', 'chapped', 'chapter', 'charily', 'charier', 'charity', 'charade', 'charged', 'charger', 'charges', 'chariot', 'charley', 'charmed', 'charmer', 'charnel', 'charred', 'charted', 'charter', 'chasing', 'chasers', 'chasmal', 'chasses', 'chassid', 'chassis', 'chaster', 'chasten', 'chateau', 'chatted', 'chatter', 'chattel', 'cheaper', 'cheaply', 'cheapen', 'cheated', 'cheater', 'checked', 'checker', 'checkup', 'cheddar', 'cheeped', 'cheered', 'cheeses', 'cheetah', 'chemist', 'chemins', 'chemise', 'cherish', 'cheroot', 'cherubs', 'chervil', 'chesses', 'cheviot', 'chevron', 'chewing', 'chewier', 'chewink', 'chianti', 'chicles', 'chicago', 'chicano', 'chicken', 'chicory', 'chiding', 'chidden', 'chiefly', 'chiffon', 'chigger', 'chignon', 'chilean', 'chilies', 'chilled', 'chiming', 'chimera', 'chimney', 'chinese', 'chinked', 'chinned', 'chinook', 'chintzy', 'chipped', 'chipper', 'chirped', 'chirrup', 'chisels', 'chitins', 'chitons', 'chitter', 'chloral', 'chocked', 'choicer', 'choices', 'choking', 'chokers', 'cholers', 'cholera', 'chooses', 'chopped', 'chopper', 'chorion', 'chorale', 'choreas', 'chorine', 'choroid', 'chortle', 'chowder', 'chrisms', 'chromed', 'chromes', 'chromic', 'chronic', 'chucked', 'chuckle', 'chugged', 'chukkas', 'chummed', 'chunked', 'churned', 'chutney', 'ciliary', 'ciliate', 'cinched', 'cinches', 'cinders', 'cinemas', 'ciphers', 'circles', 'circled', 'circlet', 'circuit', 'cirques', 'cistern', 'citadel', 'citizen', 'citrate', 'citrons', 'civvies', 'clacked', 'claimed', 'clamors', 'clamber', 'clammed', 'clamped', 'clanged', 'clangor', 'clanked', 'clapped', 'clapper', 'claques', 'clarion', 'clarity', 'clarets', 'clarify', 'clashed', 'clashes', 'clasped', 'classed', 'classes', 'classic', 'clatter', 'clauses', 'clavier', 'clawing', 'cleaned', 'cleaner', 'cleanly', 'cleanse', 'cleanup', 'cleared', 'clearer', 'clearly', 'cleaved', 'cleaver', 'cleaves', 'clement', 'clerics', 'clerked', 'cliches', 'clicked', 'clicker', 'clients', 'climate', 'climbed', 'climber', 'clinics', 'clinked', 'clinker', 'clipped', 'clipper', 'cliquey', 'cliques', 'cloacae', 'cloaked', 'clobber', 'cloches', 'clocked', 'clogged', 'cloning', 'clopped', 'closing', 'closest', 'closely', 'closets', 'closure', 'clothed', 'clothes', 'clotted', 'cloture', 'clouded', 'clouted', 'clovers', 'clowned', 'cloying', 'clubbed', 'clucked', 'clueing', 'clumber', 'clumped', 'cluster', 'clutter', 'coaches', 'coarser', 'coarsen', 'coasted', 'coaster', 'coastal', 'coating', 'coaxing', 'coaxial', 'cobalts', 'cobbles', 'cobbled', 'cobbler', 'cobwebs', 'cocaine', 'coccoid', 'cochlea', 'cocking', 'cockers', 'cockily', 'cockier', 'cockles', 'cockade', 'cockled', 'cockney', 'cockpit', 'coconut', 'cocoons', 'coddles', 'coddled', 'codeine', 'codfish', 'codgers', 'codices', 'codicil', 'codling', 'coequal', 'coerced', 'coerces', 'coevals', 'coexist', 'coffers', 'coffees', 'coffins', 'cogency', 'cognacs', 'cognate', 'cohabit', 'cohered', 'coheres', 'cohorts', 'coifing', 'coiling', 'coining', 'coinage', 'coition', 'coldest', 'colicky', 'colitis', 'collage', 'collars', 'collard', 'collate', 'collect', 'colleen', 'college', 'collies', 'collide', 'collier', 'colloid', 'collude', 'cologne', 'colombo', 'colonel', 'colored', 'colossi', 'coltish', 'columns', 'combing', 'combats', 'combine', 'combust', 'comedic', 'comedos', 'comfier', 'comfits', 'comfort', 'comical', 'comings', 'command', 'commend', 'comment', 'commits', 'commode', 'commons', 'commune', 'commute', 'comoros', 'compact', 'company', 'compare', 'compass', 'compeer', 'compels', 'compete', 'compile', 'complex', 'complin', 'comport', 'compose', 'compost', 'compote', 'compute', 'comrade', 'comsats', 'concave', 'conceal', 'concede', 'conceit', 'concept', 'concern', 'concert', 'concise', 'concoct', 'concord', 'concurs', 'condors', 'condemn', 'condign', 'condole', 'condoms', 'condone', 'conduce', 'conduct', 'conduit', 'confabs', 'confers', 'confess', 'confide', 'confine', 'confirm', 'conform', 'confuse', 'confute', 'congeal', 'congers', 'congest', 'conical', 'conifer', 'conjoin', 'conjure', 'conking', 'conning', 'connate', 'connect', 'connive', 'connote', 'conquer', 'consent', 'consign', 'consist', 'console', 'consort', 'consuls', 'consult', 'consume', 'contact', 'contain', 'contemn', 'contend', 'content', 'contest', 'context', 'contort', 'contour', 'contras', 'control', 'contuse', 'convene', 'convent', 'convert', 'conveys', 'convict', 'convoke', 'convoys', 'cooking', 'cookers', 'cookies', 'cookery', 'cookout', 'cooling', 'coolers', 'coolies', 'coolest', 'coolant', 'cooping', 'coopers', 'cooties', 'copepod', 'copiers', 'copious', 'copilot', 'copping', 'coppers', 'coppery', 'coppice', 'copters', 'copulas', 'copular', 'copying', 'copyist', 'copycat', 'coquina', 'coracle', 'corbels', 'cording', 'cordage', 'cordial', 'cordite', 'cordons', 'corking', 'corkers', 'corkier', 'corning', 'corners', 'cornier', 'corncob', 'corneal', 'corneas', 'cornets', 'cornice', 'cornish', 'corolla', 'coroner', 'coronas', 'coronet', 'corpora', 'corpses', 'corrals', 'correct', 'corrode', 'corrupt', 'corsage', 'corsair', 'corsets', 'corsica', 'cortege', 'corvine', 'coryzas', 'cosigns', 'cosines', 'cossack', 'cossets', 'costing', 'costive', 'costars', 'costume', 'coterie', 'cotters', 'cottage', 'cottons', 'cottony', 'couched', 'couches', 'cougars', 'coughed', 'coulees', 'coulomb', 'council', 'counsel', 'counted', 'counter', 'country', 'couples', 'coupled', 'coupler', 'couplet', 'coupons', 'courage', 'courier', 'coursed', 'courser', 'courses', 'courted', 'courtly', 'cousins', 'couture', 'covered', 'coverts', 'coveted', 'cowards', 'cowbell', 'cowbird', 'cowboys', 'cowered', 'cowgirl', 'cowhand', 'cowherd', 'cowhide', 'cowling', 'cowlick', 'cowpoke', 'cowries', 'cowskin', 'cowslip', 'coxcomb', 'coyness', 'coyotes', 'cozened', 'coziest', 'crabbed', 'cracked', 'cracker', 'crackle', 'crackup', 'cradles', 'cradled', 'crafted', 'cragged', 'crammed', 'cramped', 'crampon', 'craning', 'cranial', 'cranium', 'cranked', 'crappie', 'crashed', 'crashes', 'crasser', 'crating', 'craters', 'craving', 'cravats', 'cravens', 'crawled', 'crayons', 'crazing', 'crazily', 'crazier', 'creaked', 'creamed', 'creamer', 'creased', 'creases', 'created', 'creates', 'creator', 'creches', 'credits', 'creeper', 'cremate', 'crenate', 'creoles', 'creosol', 'cresols', 'cresses', 'crested', 'cretans', 'cretins', 'crevice', 'crewing', 'crewels', 'crewmen', 'crewman', 'cribbed', 'cricked', 'cricket', 'crimean', 'crimped', 'crimson', 'cringed', 'cringes', 'crinkle', 'cripple', 'crisped', 'crisper', 'crisply', 'critics', 'critter', 'croaked', 'crochet', 'crofter', 'cronies', 'crooked', 'crooned', 'crooner', 'cropped', 'cropper', 'croquet', 'crosier', 'crossed', 'crosses', 'crouton', 'crowing', 'crowbar', 'crowded', 'crowned', 'crucial', 'crucify', 'crudity', 'crudest', 'crudely', 'crueler', 'cruelly', 'cruelty', 'cruised', 'cruiser', 'cruises', 'cruller', 'crumbed', 'crumbly', 'crumble', 'crumple', 'crumpet', 'crunchy', 'crusade', 'crushed', 'crusher', 'crushes', 'crusted', 'crybaby', 'cryogen', 'cryptic', 'crystal', 'cubical', 'cubicle', 'cubisms', 'cubists', 'cuckold', 'cuckoos', 'cuddles', 'cuddled', 'cudgels', 'cuffing', 'cuirass', 'cuisine', 'culling', 'culprit', 'cultist', 'culture', 'culvert', 'cumbers', 'cumulus', 'cunning', 'cupcake', 'cupfuls', 'cupolas', 'cupping', 'cuprous', 'curably', 'curable', 'curares', 'curates', 'curator', 'curbing', 'curdles', 'curdled', 'curette', 'curfews', 'curious', 'curling', 'curlers', 'curlier', 'curlews', 'currant', 'current', 'curried', 'curries', 'cursing', 'cursers', 'cursors', 'cursive', 'cursory', 'curtest', 'curtail', 'curtain', 'curving', 'curvets', 'cushion', 'cuspids', 'cussing', 'custard', 'custody', 'customs', 'cutaway', 'cutback', 'cuticle', 'cutlass', 'cutlery', 'cutlets', 'cutoffs', 'cutouts', 'cutters', 'cutting', 'cutworm', 'cyanide', 'cycling', 'cyclist', 'cycloid', 'cyclone', 'cyclops', 'cygnets', 'cymbals', 'cynical', 'cypress', 'cypriot', 'czarist', 'czarism', 'czarina', 'dabbing', 'dabbles', 'dabbled', 'dabbler', 'dactyls', 'daddies', 'daffier', 'daftest', 'daggers', 'dahlias', 'dailies', 'dairies', 'daisies', 'dakotas', 'dallies', 'dallied', 'damaged', 'damages', 'damasks', 'damming', 'damning', 'damping', 'dampers', 'dampest', 'dampens', 'damsels', 'damsons', 'dancing', 'dancers', 'danders', 'dandily', 'dandies', 'dandles', 'dandify', 'dandled', 'dangles', 'dangers', 'dangled', 'dankest', 'daphnia', 'dapples', 'dappled', 'darkest', 'darkens', 'darling', 'darning', 'darnels', 'darting', 'darters', 'dartles', 'dartled', 'dashing', 'dashers', 'dashiki', 'datable', 'datives', 'daubing', 'daubers', 'daunted', 'dauphin', 'dawdles', 'dawdled', 'dawning', 'daybook', 'daylong', 'daystar', 'daytime', 'dazedly', 'dazzles', 'dazzled', 'deacons', 'deadest', 'deadens', 'deadpan', 'deafest', 'deafens', 'dealing', 'dealers', 'deanery', 'dearest', 'dearths', 'deathly', 'debacle', 'debarks', 'debased', 'debases', 'debated', 'debates', 'debauch', 'debited', 'debrief', 'debtors', 'debunks', 'debuted', 'decades', 'decagon', 'decamps', 'decants', 'decayed', 'decease', 'deceits', 'deceive', 'decency', 'decibel', 'decides', 'decided', 'decimal', 'decking', 'deckles', 'deckled', 'declaim', 'declare', 'decline', 'decocts', 'decoded', 'decoder', 'decodes', 'decorum', 'decoyed', 'decreed', 'decrees', 'decried', 'decries', 'decrypt', 'deduced', 'deduces', 'deducts', 'deeding', 'deeming', 'deepest', 'deepens', 'defaced', 'defaces', 'defamed', 'defames', 'default', 'defeats', 'defects', 'defends', 'defense', 'defiant', 'deficit', 'defiled', 'defiles', 'defined', 'definer', 'defines', 'deflate', 'deflect', 'deforms', 'defraud', 'defrays', 'defrock', 'defrost', 'deftest', 'defunct', 'defused', 'defuses', 'defying', 'degauss', 'degrade', 'degrees', 'dehisce', 'deicing', 'deicers', 'deified', 'deifies', 'deigned', 'deistic', 'deities', 'dejects', 'delayed', 'deleted', 'deletes', 'delicti', 'delight', 'delilah', 'delimit', 'deliver', 'delouse', 'deltaic', 'deltoid', 'deluded', 'deludes', 'deluged', 'deluges', 'delving', 'demands', 'demeans', 'demerit', 'demerol', 'demesne', 'demigod', 'demised', 'demises', 'demonic', 'demoted', 'demotes', 'demount', 'demurer', 'dengues', 'deniers', 'denials', 'denizen', 'denmark', 'denoted', 'denotes', 'density', 'densest', 'densely', 'denting', 'dentist', 'dentate', 'dentine', 'denture', 'denuded', 'denudes', 'denying', 'departs', 'depends', 'depicts', 'deplane', 'deplete', 'deplore', 'deploys', 'deports', 'deposed', 'deposes', 'deposal', 'deposit', 'deprave', 'depress', 'deprive', 'deputed', 'deputes', 'derails', 'derange', 'derbies', 'derided', 'derides', 'derived', 'derives', 'derrick', 'dervish', 'desalts', 'descant', 'descend', 'descent', 'deserts', 'deserve', 'designs', 'desired', 'desires', 'desists', 'desktop', 'despair', 'despise', 'despite', 'despoil', 'despond', 'despots', 'dessert', 'destine', 'destiny', 'destroy', 'details', 'detains', 'detects', 'detente', 'detests', 'detours', 'detract', 'detrain', 'detroit', 'devalue', 'develop', 'deviant', 'deviate', 'devices', 'deviled', 'devious', 'devised', 'devises', 'devolve', 'devotes', 'devotee', 'devoted', 'devours', 'dewdrop', 'dewiest', 'dewlaps', 'dextral', 'dextrin', 'diadems', 'diagram', 'dialing', 'dialers', 'dialect', 'dialogs', 'dialyze', 'diamond', 'diapers', 'diaries', 'diarist', 'diatoms', 'dibasic', 'dibbles', 'dibbled', 'dickers', 'dickies', 'dickens', 'dickeys', 'diction', 'dictate', 'diddles', 'diddled', 'diesels', 'dieting', 'dietary', 'diethyl', 'differs', 'diffuse', 'digests', 'digging', 'diggers', 'digital', 'dignity', 'dignify', 'digraph', 'digress', 'dilated', 'dilates', 'dilator', 'dilemna', 'dillies', 'diluted', 'dilutes', 'dimming', 'dimmers', 'dimmest', 'dimness', 'dimorph', 'dimples', 'dimpled', 'dimwits', 'dinette', 'dinging', 'dingier', 'dingbat', 'dingoes', 'dinkier', 'dinning', 'dinners', 'diocese', 'diorama', 'diorite', 'dioxide', 'diploid', 'diploma', 'dipoles', 'dipping', 'dippers', 'diptych', 'directs', 'direful', 'dirndls', 'dirtier', 'dirties', 'dirtied', 'disable', 'disarms', 'disavow', 'disband', 'disbars', 'disbuds', 'discard', 'discern', 'discoid', 'discord', 'discuss', 'disdain', 'disease', 'disgust', 'dishing', 'dishpan', 'dishrag', 'disjoin', 'dislike', 'dismays', 'dismiss', 'disobey', 'disowns', 'dispels', 'display', 'disport', 'dispose', 'dispute', 'disrobe', 'disrupt', 'dissect', 'dissent', 'distaff', 'distant', 'distend', 'distill', 'distort', 'disturb', 'disused', 'ditched', 'ditches', 'dithers', 'ditties', 'diurnal', 'diverge', 'diverse', 'diverts', 'divests', 'divided', 'divider', 'divides', 'divined', 'diviner', 'divines', 'divisor', 'divorce', 'divulge', 'divvied', 'divvies', 'dizzied', 'dizzier', 'dizzies', 'dizzily', 'dobbins', 'docents', 'docking', 'dockage', 'dockets', 'doctors', 'dodders', 'dodging', 'dodgers', 'doeskin', 'doffing', 'dogcart', 'dogfish', 'dogging', 'doggies', 'dogtrot', 'dogwood', 'doilies', 'doleful', 'dolling', 'dollies', 'dollars', 'dollops', 'dolmans', 'dolmens', 'dolphin', 'doltish', 'domains', 'donated', 'donates', 'donator', 'donjons', 'donkeys', 'donning', 'donnish', 'doodles', 'doodads', 'doodled', 'dooming', 'doormen', 'doorman', 'doormat', 'doorway', 'dopiest', 'doppler', 'dormers', 'dormant', 'dormice', 'dosages', 'dossier', 'dotages', 'dotards', 'dotting', 'dottier', 'dottles', 'doubles', 'doubled', 'doublet', 'doubted', 'doubter', 'douched', 'douches', 'doughty', 'douglas', 'dourest', 'dousing', 'dowager', 'dowdier', 'doweled', 'dowered', 'downing', 'downers', 'downier', 'dowries', 'dowsing', 'dowsers', 'drabber', 'drafted', 'draftee', 'dragged', 'dragger', 'draggle', 'dragnet', 'dragons', 'dragoon', 'drained', 'draping', 'drapery', 'drastic', 'drawing', 'drawers', 'drawled', 'drayage', 'dreaded', 'dreamed', 'dreamer', 'dredged', 'dredges', 'dresden', 'dressed', 'dresser', 'dresses', 'dribble', 'driblet', 'drifted', 'drifter', 'drilled', 'drinker', 'dripped', 'driving', 'drivers', 'drizzly', 'drizzle', 'drogues', 'droller', 'droning', 'drooled', 'drooped', 'droplet', 'dropout', 'dropped', 'dropper', 'drought', 'drovers', 'drowned', 'drowsed', 'drowses', 'drubbed', 'drudged', 'drudges', 'drugged', 'drumlin', 'drummed', 'drummer', 'drunken', 'dryness', 'dualism', 'duality', 'dualist', 'dubbing', 'dubiety', 'dubious', 'duchies', 'duchess', 'ducking', 'duckpin', 'ductile', 'dudgeon', 'dueling', 'duelers', 'duelist', 'duffers', 'duffels', 'dugongs', 'dugouts', 'dukedom', 'dulling', 'dullest', 'dullard', 'dumbest', 'dummies', 'dumping', 'dumpier', 'dungeon', 'dunking', 'dunning', 'duodena', 'durance', 'durably', 'durable', 'duskily', 'duskier', 'dusting', 'dusters', 'dustier', 'dustbin', 'dustpan', 'dustups', 'duteous', 'dutiful', 'dwarfed', 'dwelled', 'dweller', 'dwindle', 'dynamic', 'dynamos', 'dynasts', 'dynasty', 'eagerly', 'eaglets', 'earache', 'eardrum', 'earflap', 'earfuls', 'earlier', 'earlaps', 'earldom', 'earlobe', 'earmark', 'earmuff', 'earning', 'earners', 'earnest', 'earring', 'earshot', 'earthly', 'earthen', 'earwigs', 'easiest', 'easters', 'eastern', 'eatable', 'ebbings', 'ebonies', 'ebonite', 'echelon', 'echidna', 'echoing', 'eclairs', 'eclipse', 'eclogue', 'ecocide', 'ecology', 'economy', 'ecotype', 'ecstasy', 'ecuador', 'eczemas', 'eddying', 'edgiest', 'edgings', 'edibles', 'edified', 'edifies', 'edifice', 'editing', 'edition', 'editors', 'educing', 'educate', 'eeriest', 'effaced', 'effaces', 'effects', 'efforts', 'effused', 'effuses', 'egghead', 'eggnogs', 'egoists', 'egotist', 'egotism', 'eighths', 'ejected', 'ejector', 'elapsed', 'elapses', 'elastic', 'elating', 'elation', 'elbowed', 'elderly', 'elected', 'elector', 'elegies', 'elegant', 'elegiac', 'elegize', 'element', 'elevate', 'elevens', 'elicits', 'eliding', 'elision', 'elitist', 'elitism', 'elixirs', 'ellipse', 'eloping', 'eluding', 'eluders', 'elusion', 'elusive', 'elysian', 'emanate', 'embalms', 'embanks', 'embargo', 'embarks', 'embassy', 'emblems', 'embolic', 'embolus', 'embosom', 'embrace', 'embroil', 'embryos', 'emended', 'emeries', 'emerald', 'emerged', 'emerges', 'emeriti', 'emetics', 'emigres', 'eminent', 'emirate', 'emitted', 'emitter', 'emoting', 'emotive', 'emotion', 'empathy', 'emperor', 'empires', 'employs', 'emporia', 'empower', 'empress', 'emptied', 'emptier', 'empties', 'emptily', 'emulate', 'emulous', 'enables', 'enabled', 'enacted', 'enactor', 'enamels', 'enamors', 'encamps', 'encased', 'encases', 'enchain', 'enchant', 'enclave', 'enclose', 'encoded', 'encoder', 'encodes', 'encored', 'encores', 'encrust', 'encrypt', 'endears', 'endemic', 'endings', 'endives', 'endless', 'endorse', 'endowed', 'enduing', 'endured', 'endures', 'endwise', 'enemies', 'enfolds', 'enforce', 'engages', 'engaged', 'engines', 'engirds', 'england', 'english', 'engorge', 'engraft', 'engrave', 'engross', 'engulfs', 'enhance', 'enigmas', 'enjoins', 'enjoyed', 'enlaced', 'enlaces', 'enlarge', 'enlists', 'enliven', 'ennoble', 'enplane', 'enraged', 'enrages', 'enrolls', 'ensiles', 'ensigns', 'ensiled', 'enslave', 'ensnare', 'ensuing', 'ensured', 'ensures', 'entails', 'entente', 'entered', 'enteric', 'enthuse', 'entices', 'enticed', 'entitle', 'entombs', 'entries', 'entrain', 'entrant', 'entraps', 'entrees', 'entreat', 'entropy', 'entrust', 'entwine', 'envelop', 'envenom', 'envious', 'envying', 'enwraps', 'enzymes', 'eoliths', 'epaulet', 'epicene', 'epicure', 'epigram', 'episode', 'epistle', 'epitaph', 'epithet', 'epitome', 'epochal', 'eponyms', 'epoxies', 'epsilon', 'equably', 'equable', 'equaled', 'equally', 'equated', 'equates', 'equator', 'equerry', 'equines', 'equinox', 'erasing', 'erasers', 'erasure', 'erected', 'erector', 'eremite', 'ermines', 'eroding', 'erosion', 'erosive', 'erotica', 'errands', 'erratic', 'erratum', 'eructed', 'erudite', 'erupted', 'escaped', 'escapes', 'escapee', 'eschews', 'escorts', 'escrows', 'eskimos', 'esparto', 'espouse', 'espying', 'esquire', 'essayed', 'essence', 'estates', 'esteems', 'esthete', 'estonia', 'estrous', 'estuary', 'etching', 'eternal', 'ethanes', 'ethanol', 'ethical', 'ethnics', 'ethoses', 'euchres', 'eunuchs', 'euphony', 'eurasia', 'evacuee', 'evading', 'evasion', 'evasive', 'evening', 'evicted', 'evictor', 'evident', 'evinced', 'evinces', 'evoking', 'evolved', 'evolves', 'exacted', 'exactor', 'exactly', 'exalted', 'examine', 'example', 'exceeds', 'excepts', 'excerpt', 'excised', 'excises', 'excites', 'excited', 'exclaim', 'exclave', 'exclude', 'excrete', 'excreta', 'excused', 'excuses', 'execute', 'exempts', 'exerted', 'exhaled', 'exhales', 'exhaust', 'exhibit', 'exhorts', 'exhumed', 'exhumes', 'exigent', 'exiling', 'existed', 'exiting', 'exotics', 'expands', 'expanse', 'expects', 'expends', 'expense', 'experts', 'expiate', 'expired', 'expires', 'explain', 'explode', 'exploit', 'explore', 'exports', 'exposed', 'exposes', 'expound', 'express', 'expunge', 'extends', 'extents', 'extinct', 'extorts', 'extract', 'extreme', 'extrude', 'exuding', 'exulted', 'eyeball', 'eyebolt', 'eyebrow', 'eyecups', 'eyefuls', 'eyelash', 'eyelets', 'eyelids', 'eyeshot', 'eyesore', 'eyespot', 'eyewash', 'fabling', 'fabrics', 'facades', 'faceted', 'facials', 'facings', 'faction', 'factors', 'factory', 'factual', 'faculty', 'faddist', 'faddish', 'fagging', 'fagoted', 'faience', 'failing', 'failles', 'failure', 'fainted', 'fainter', 'faintly', 'fairies', 'fairest', 'fairway', 'falcons', 'falling', 'fallacy', 'fallout', 'fallows', 'falsity', 'falsest', 'falsely', 'falsify', 'falters', 'famines', 'fanatic', 'fancied', 'fancier', 'fancies', 'fanfare', 'fanning', 'fannies', 'fantail', 'fantasy', 'faraday', 'faraway', 'farceur', 'farinas', 'farming', 'farmers', 'farrago', 'farrows', 'farther', 'fascist', 'fascism', 'fasciae', 'fashion', 'fasting', 'fastest', 'fastens', 'fatales', 'fatally', 'fatback', 'fateful', 'fathead', 'fathers', 'fathoms', 'fatigue', 'fatting', 'fattier', 'fattest', 'fattens', 'fatuity', 'fatuous', 'faucets', 'faulted', 'fauvism', 'favored', 'fawning', 'fearing', 'fearful', 'feasted', 'feather', 'feature', 'febrile', 'federal', 'fedoras', 'feebler', 'feeding', 'feeders', 'feedbag', 'feeling', 'feelers', 'feigned', 'feinted', 'felines', 'felling', 'fellest', 'fellows', 'felting', 'females', 'femoral', 'fencing', 'fencers', 'fending', 'fenders', 'fennels', 'ferment', 'fermium', 'fernery', 'ferries', 'ferrous', 'ferrets', 'ferried', 'ferrite', 'ferrule', 'fertile', 'ferules', 'feruled', 'fervors', 'fervent', 'festers', 'festive', 'festoon', 'fetched', 'fetches', 'fetlock', 'fetters', 'fettles', 'fettled', 'fetuses', 'feuding', 'fevered', 'fiances', 'fiancee', 'fibbing', 'fibbers', 'fibrils', 'fibrins', 'fibroid', 'fibrous', 'fibulae', 'fickler', 'fiction', 'fictive', 'fiddles', 'fiddled', 'fiddler', 'fidgets', 'fidgety', 'fielded', 'fielder', 'fierier', 'fiercer', 'fiestas', 'fifties', 'fifteen', 'fighter', 'figment', 'figured', 'figures', 'fijians', 'filbert', 'filched', 'filches', 'fileted', 'filings', 'filling', 'fillers', 'fillies', 'fillets', 'fillips', 'filming', 'filmier', 'filters', 'finance', 'finagle', 'finales', 'finally', 'finches', 'finding', 'finders', 'finesse', 'fingers', 'finials', 'finical', 'finicky', 'finings', 'finites', 'finking', 'finland', 'finning', 'finnier', 'finnish', 'firearm', 'firebox', 'firebug', 'firedog', 'firefly', 'firemen', 'fireman', 'firings', 'firkins', 'firming', 'firmest', 'firstly', 'fishing', 'fishers', 'fishily', 'fishier', 'fishery', 'fisheye', 'fishnet', 'fissile', 'fission', 'fissure', 'fisting', 'fistful', 'fistula', 'fitness', 'fitting', 'fitters', 'fittest', 'fixable', 'fixated', 'fixates', 'fixedly', 'fixings', 'fixture', 'fizzing', 'fizzles', 'fizzled', 'flaccid', 'flacons', 'flagged', 'flagons', 'flailed', 'flaking', 'flakily', 'flakier', 'flaming', 'flanged', 'flanges', 'flanked', 'flanker', 'flannel', 'flapped', 'flapper', 'flaring', 'flashed', 'flashes', 'flatcar', 'flatted', 'flatten', 'flatter', 'flattop', 'flaunts', 'flavors', 'flawing', 'flaying', 'flecked', 'fledged', 'fledges', 'fleeing', 'fleeced', 'fleeces', 'fleeted', 'fleeter', 'fleshed', 'fleshes', 'fleshly', 'flexing', 'flexion', 'flexors', 'flexure', 'flicked', 'flicker', 'flights', 'flighty', 'flipped', 'flipper', 'flirted', 'flitted', 'floated', 'floater', 'flocked', 'flogged', 'flooded', 'floored', 'flopped', 'florist', 'florals', 'florets', 'florida', 'florins', 'flossed', 'flosses', 'flotsam', 'flounce', 'floured', 'flouted', 'flowing', 'flowers', 'flowery', 'fluency', 'fluffed', 'fluking', 'flukier', 'fluming', 'flummox', 'flunked', 'flushed', 'flushes', 'fluster', 'fluting', 'flutist', 'flutter', 'fluvial', 'fluxing', 'flyaway', 'flyleaf', 'flytrap', 'foaling', 'foaming', 'foamier', 'fobbing', 'focally', 'focused', 'focuses', 'fodders', 'fogging', 'foggily', 'foggier', 'foghorn', 'foibles', 'foiling', 'foisted', 'folding', 'folders', 'foliage', 'foliate', 'folioed', 'folkway', 'follies', 'follows', 'foments', 'fondest', 'fondles', 'fondant', 'fondled', 'fondues', 'fooling', 'foolery', 'foolish', 'footing', 'footage', 'footmen', 'footman', 'foppery', 'foppish', 'foraged', 'forager', 'forages', 'foramen', 'forayed', 'forbade', 'forbear', 'forbids', 'forbore', 'forcing', 'forceps', 'fording', 'forearm', 'foreign', 'foreleg', 'foremen', 'foreman', 'foresaw', 'foresee', 'forests', 'forever', 'forfeit', 'forging', 'forgave', 'forgers', 'forgery', 'forgets', 'forgive', 'forgoer', 'forgoes', 'forgone', 'forking', 'forkful', 'forlorn', 'forming', 'formers', 'formals', 'formats', 'formica', 'formula', 'forsake', 'forsook', 'forties', 'fortify', 'fortune', 'forward', 'forwent', 'fossils', 'fosters', 'fouling', 'foulest', 'foulard', 'founded', 'founder', 'foundry', 'fourths', 'fowling', 'foxhole', 'foxiest', 'fragile', 'frailer', 'frailty', 'framing', 'francas', 'franked', 'franker', 'frankly', 'frantic', 'frappes', 'fraught', 'fraying', 'frazzle', 'freaked', 'freckle', 'freeing', 'freebie', 'freedom', 'freemen', 'freeman', 'freesia', 'freeway', 'freezer', 'freezes', 'freight', 'fresher', 'freshly', 'freshen', 'freshet', 'fretful', 'fretted', 'friable', 'fridays', 'friends', 'friezes', 'frigate', 'frights', 'frilled', 'fringed', 'fringes', 'frisbee', 'frisked', 'fritter', 'frizzed', 'frizzes', 'frizzle', 'frocked', 'frogmen', 'frogman', 'frolics', 'fronted', 'frontal', 'frosted', 'frothed', 'frowned', 'fruited', 'frustum', 'fuchsia', 'fuddles', 'fuddled', 'fudging', 'fueling', 'fuhrers', 'fulcrum', 'fulfill', 'fulling', 'fullers', 'fullest', 'fulsome', 'fumbles', 'fumbled', 'fumbler', 'funding', 'funeral', 'fungous', 'funkier', 'funning', 'funnily', 'funnier', 'funnies', 'funnels', 'furbish', 'furious', 'furling', 'furlong', 'furnace', 'furnish', 'furring', 'furrier', 'furrows', 'furtive', 'further', 'fusible', 'fusions', 'fussing', 'fussily', 'fussier', 'fustier', 'fustian', 'futures', 'fuzzing', 'fuzzily', 'fuzzier', 'gabbing', 'gabbers', 'gabbier', 'gabbles', 'gabbled', 'gabfest', 'gadding', 'gadgets', 'gaffing', 'gagging', 'gaggles', 'gaggled', 'gaining', 'gainers', 'gainful', 'gainsay', 'gaiters', 'galenas', 'galilee', 'galling', 'gallant', 'galleon', 'gallery', 'galleys', 'gallium', 'gallons', 'gallops', 'gallows', 'gambles', 'gambits', 'gambled', 'gambler', 'gambols', 'gametes', 'gamiest', 'gammons', 'ganders', 'ganging', 'ganglia', 'gangues', 'gangway', 'gannets', 'gantlet', 'gapping', 'garaged', 'garages', 'garbing', 'garbles', 'garbage', 'garbled', 'gardens', 'garfish', 'gargles', 'gargled', 'garlics', 'garland', 'garment', 'garners', 'garnets', 'garnish', 'garrets', 'garrote', 'garters', 'gaseous', 'gashing', 'gaskets', 'gasohol', 'gasping', 'gassing', 'gassers', 'gassier', 'gastric', 'gateway', 'gathers', 'gatling', 'gaudily', 'gaudier', 'gauging', 'gaunter', 'gauzier', 'gaveled', 'gavials', 'gavotte', 'gawking', 'gawkily', 'gawkier', 'gazebos', 'gazelle', 'gazette', 'gearing', 'gearbox', 'geezers', 'gefilte', 'geishas', 'gelatin', 'gelding', 'gemming', 'gemsbok', 'genders', 'generic', 'general', 'geneses', 'genesis', 'genetic', 'genitor', 'genital', 'gentles', 'genteel', 'gentian', 'gentile', 'gentled', 'gentler', 'genuine', 'geodesy', 'geology', 'georgia', 'gerbils', 'germans', 'germane', 'germany', 'gerunds', 'gessoes', 'gestalt', 'gestapo', 'gestate', 'gesture', 'getable', 'getaway', 'getting', 'getters', 'gewgaws', 'geysers', 'ghastly', 'gherkin', 'ghettos', 'ghosted', 'ghostly', 'gibbers', 'gibbous', 'gibbets', 'gibbons', 'giblets', 'giddied', 'giddier', 'giddies', 'gifting', 'gigabit', 'gigging', 'giggles', 'giggled', 'gigolos', 'gilding', 'gilling', 'gimbals', 'gimlets', 'gimmick', 'gimping', 'gimpier', 'gingers', 'gingham', 'ginning', 'ginseng', 'giraffe', 'girding', 'girders', 'girdles', 'girdled', 'girlish', 'girting', 'gizzard', 'glaceed', 'glacial', 'glacier', 'gladder', 'gladden', 'glamour', 'glanced', 'glances', 'glaring', 'glassed', 'glasses', 'glazing', 'glazier', 'gleamed', 'gleaned', 'gleeful', 'glibber', 'gliding', 'gliders', 'glimmer', 'glimpse', 'glinted', 'glister', 'glisten', 'glitter', 'gloated', 'globing', 'globate', 'globule', 'gloomed', 'gloried', 'glories', 'glorify', 'glossed', 'glosses', 'glottal', 'glottis', 'gloving', 'glowing', 'glowers', 'glozing', 'glucose', 'glummer', 'glutens', 'gluteus', 'glutted', 'glutton', 'glycols', 'gnarled', 'gnashed', 'gnashes', 'gnawing', 'gnocchi', 'gnomons', 'gnostic', 'goading', 'goalies', 'goatees', 'gobbles', 'gobbled', 'gobbler', 'goblets', 'goblins', 'goddess', 'godhead', 'godlier', 'godless', 'godlike', 'godsend', 'godsons', 'goggles', 'goggled', 'goiters', 'golfing', 'golfers', 'goliath', 'gondola', 'goobers', 'goodies', 'goofing', 'goofier', 'googols', 'gooiest', 'gooneys', 'goosing', 'gophers', 'gorging', 'goriest', 'gorilla', 'gorings', 'goshawk', 'gosling', 'gospels', 'gossips', 'gossipy', 'gouache', 'gouging', 'goulash', 'gourmet', 'goutier', 'governs', 'gowning', 'grabbed', 'grabber', 'gracing', 'grackle', 'grading', 'graders', 'gradate', 'gradual', 'grafted', 'grained', 'grammar', 'grampus', 'granary', 'grander', 'grandly', 'grandam', 'grandee', 'grandma', 'grandpa', 'granger', 'granges', 'granite', 'granola', 'granted', 'granter', 'grantor', 'grantee', 'granule', 'graphed', 'graphic', 'grapnel', 'grapple', 'grasped', 'grassed', 'grasses', 'graters', 'gratify', 'grating', 'graving', 'gravers', 'gravies', 'gravity', 'gravest', 'gravely', 'gravels', 'gravure', 'graying', 'grayest', 'grayish', 'grazing', 'grazier', 'greased', 'greaser', 'greases', 'greater', 'greatly', 'grecian', 'greened', 'greenly', 'greener', 'greeted', 'greeter', 'gremlin', 'grenade', 'grenada', 'griddle', 'grieved', 'grieves', 'griffin', 'grilled', 'grilles', 'griming', 'grimier', 'grimace', 'grimmer', 'grinder', 'gringos', 'grinned', 'griping', 'gripped', 'grippes', 'gristly', 'gristle', 'gritted', 'grizzly', 'groaned', 'grocers', 'grocery', 'grogram', 'groined', 'grommet', 'groomed', 'grooved', 'grooves', 'groping', 'grossed', 'grosser', 'grosses', 'grossly', 'grouchy', 'grounds', 'grouped', 'grouper', 'groupie', 'groused', 'grouses', 'grouted', 'grovels', 'growing', 'growers', 'growled', 'grownup', 'growths', 'grubbed', 'grudged', 'grudges', 'gruffer', 'gruffly', 'grumble', 'grumped', 'grunion', 'grunted', 'gruyere', 'guanaco', 'guanine', 'guarded', 'gudgeon', 'guessed', 'guesses', 'guested', 'guffaws', 'guiding', 'guidons', 'guineas', 'guitars', 'gulches', 'gulfing', 'gulling', 'gullies', 'gullets', 'gullied', 'gulping', 'gumboil', 'gumdrop', 'gumming', 'gummier', 'gumshoe', 'gumwood', 'gunboat', 'gunfire', 'gunlock', 'gunning', 'gunnies', 'gunners', 'gunnery', 'gunshot', 'gunwale', 'guppies', 'gurgles', 'gurgled', 'gushing', 'gushers', 'gushier', 'gussets', 'gusting', 'gustier', 'gutless', 'gutsier', 'gutting', 'gutters', 'guzzles', 'guzzled', 'gymnast', 'gypping', 'gypsies', 'gypsums', 'gyrated', 'gyrates', 'gyrator', 'habited', 'habitat', 'habitue', 'hacking', 'hackers', 'hackies', 'hackles', 'hackled', 'hackney', 'hacksaw', 'haddock', 'hafnium', 'hafting', 'hagfish', 'haggles', 'haggard', 'haggled', 'haggler', 'hailing', 'hailers', 'hairier', 'haircut', 'hairdos', 'hairpin', 'haitian', 'halberd', 'halcyon', 'halfway', 'halibut', 'halides', 'halites', 'halloos', 'hallows', 'hallway', 'halogen', 'halters', 'halting', 'halving', 'halyard', 'hamlets', 'hamming', 'hammers', 'hammock', 'hampers', 'hamster', 'handing', 'handily', 'handier', 'handles', 'handbag', 'handcar', 'handful', 'handgun', 'handled', 'handler', 'handout', 'hanging', 'hangers', 'hangars', 'hangdog', 'hangmen', 'hangman', 'hangout', 'hankers', 'hansoms', 'hapless', 'haploid', 'happily', 'happier', 'happens', 'harbors', 'hardier', 'hardest', 'hardens', 'hardpan', 'hardtop', 'harelip', 'haricot', 'harking', 'harlots', 'harming', 'harmful', 'harmony', 'harness', 'harping', 'harpies', 'harpist', 'harpoon', 'harried', 'harrier', 'harries', 'harrows', 'harsher', 'harshly', 'harvest', 'hashing', 'hashish', 'hasping', 'hassles', 'hassled', 'hassock', 'hasting', 'hastily', 'hastier', 'hastens', 'hatched', 'hatches', 'hatchet', 'hateful', 'hatreds', 'hauberk', 'haughty', 'hauling', 'haulage', 'haunted', 'hautboy', 'hauteur', 'hawking', 'hawkers', 'hawsers', 'hayfork', 'hayloft', 'haymows', 'hayback', 'hayseed', 'haywire', 'hazards', 'haziest', 'hazings', 'heading', 'headers', 'headier', 'headmen', 'headman', 'headset', 'headway', 'healing', 'healers', 'healths', 'healthy', 'heaping', 'hearing', 'hearers', 'hearken', 'hearses', 'hearsay', 'hearten', 'hearths', 'heating', 'heaters', 'heathen', 'heather', 'heaving', 'heavily', 'heavier', 'heavies', 'heavens', 'hebraic', 'hebrews', 'heckles', 'heckled', 'heckler', 'hectare', 'hectors', 'hedging', 'heeding', 'heedful', 'heehaws', 'heeling', 'heftier', 'heifers', 'heights', 'heinous', 'heiress', 'heisted', 'helical', 'helicon', 'helixes', 'hellion', 'hellcat', 'hellish', 'helloed', 'helloes', 'helmets', 'helping', 'helpers', 'helpful', 'hemlock', 'hemming', 'henbane', 'hennaed', 'henpeck', 'henries', 'heparin', 'hepatic', 'heralds', 'herbals', 'herbage', 'herding', 'heretic', 'hermits', 'hernias', 'heroism', 'heroics', 'heroins', 'heroine', 'herring', 'herself', 'hexagon', 'hexanes', 'hexapod', 'heydays', 'hibachi', 'hiccups', 'hickory', 'hidalgo', 'hideous', 'hidings', 'highest', 'highboy', 'highway', 'hijacks', 'hillier', 'hillock', 'hilltop', 'himself', 'hinders', 'hinging', 'hinnies', 'hinting', 'hipbone', 'hippies', 'hippest', 'hipster', 'hirable', 'hirings', 'hirsute', 'hissing', 'history', 'hitched', 'hitches', 'hithers', 'hitting', 'hitters', 'hoagies', 'hoarier', 'hoarded', 'hoarser', 'hoaxing', 'hobbies', 'hobbles', 'hobbits', 'hobbled', 'hobbler', 'hobnail', 'hobnobs', 'hocking', 'hockeys', 'hoecake', 'hogback', 'hogging', 'hoggish', 'hogwash', 'hoisted', 'holding', 'holders', 'holdout', 'holdups', 'holiest', 'holiday', 'holisms', 'hollers', 'hollies', 'holland', 'hollows', 'holmium', 'holster', 'homages', 'hombres', 'homburg', 'homeric', 'homiest', 'hominem', 'hominid', 'homonym', 'honchos', 'honesty', 'honeyed', 'honking', 'honkers', 'honored', 'hooding', 'hoodlum', 'hoofing', 'hoofers', 'hooking', 'hookers', 'hookahs', 'hookups', 'hooping', 'hooplas', 'hoopoes', 'hoosier', 'hooting', 'hopeful', 'hopping', 'hoppers', 'horizon', 'hormone', 'horning', 'hornier', 'hornets', 'horrors', 'horrify', 'horsing', 'horsier', 'hosanna', 'hosiery', 'hospice', 'hosting', 'hostage', 'hostels', 'hostess', 'hostile', 'hostler', 'hotbeds', 'hotfoot', 'hothead', 'hotness', 'hotshot', 'hottest', 'hounded', 'housing', 'houston', 'hovered', 'howbeit', 'howdies', 'howdahs', 'however', 'howling', 'howlers', 'hoydens', 'hubbubs', 'hubcaps', 'huddles', 'huddled', 'huffing', 'huffily', 'huffier', 'hugging', 'huggers', 'hulking', 'hulling', 'humanly', 'humbled', 'humbler', 'humbles', 'humbugs', 'humdrum', 'humeral', 'humidor', 'humming', 'hummock', 'humored', 'humping', 'humuses', 'hunched', 'hunches', 'hundred', 'hungers', 'hungary', 'hunkers', 'hunting', 'hunters', 'hurdles', 'hurdled', 'hurling', 'hurlers', 'hurries', 'hurrahs', 'hurried', 'hurting', 'hurtles', 'hurtful', 'hurtled', 'husband', 'hushing', 'husking', 'huskers', 'huskily', 'huskier', 'huskies', 'hussies', 'hussars', 'hustles', 'hustled', 'hustler', 'hutches', 'hyaline', 'hyalite', 'hybrids', 'hydrant', 'hydrate', 'hydride', 'hydroid', 'hydrous', 'hygiene', 'hymenal', 'hymning', 'hymnals', 'hymnody', 'hyphens', 'hyraxes', 'hyssops', 'iambics', 'iberian', 'iceberg', 'iceboat', 'iceland', 'icicles', 'iciness', 'ideally', 'ideated', 'ideates', 'idiotic', 'idolize', 'idyllic', 'igneous', 'ignited', 'ignites', 'ignobly', 'ignoble', 'ignored', 'ignores', 'iguanas', 'ileitis', 'illegal', 'illicit', 'illness', 'illogic', 'imaging', 'imagery', 'imagine', 'imagoes', 'imbibed', 'imbibes', 'imbrued', 'imbrues', 'imbuing', 'imitate', 'immense', 'immerse', 'immoral', 'immured', 'immures', 'impacts', 'impairs', 'impaled', 'impales', 'impalas', 'impanel', 'imparts', 'impasse', 'impeach', 'impeded', 'impedes', 'impends', 'imperil', 'impetus', 'impiety', 'impinge', 'impious', 'implant', 'implied', 'implies', 'implode', 'implore', 'imports', 'imposed', 'imposes', 'imposts', 'impound', 'impress', 'imprint', 'improve', 'impugns', 'impulse', 'imputed', 'imputes', 'inanity', 'inboard', 'inbound', 'inbreed', 'incense', 'incests', 'inching', 'incised', 'incises', 'incisor', 'incited', 'incites', 'incline', 'include', 'incomes', 'incubus', 'incudes', 'indents', 'indexed', 'indexes', 'indians', 'indiana', 'indices', 'indicts', 'indigos', 'indited', 'indites', 'indoors', 'induced', 'induces', 'inducts', 'indulge', 'ineptly', 'inertly', 'inertia', 'inexact', 'infancy', 'infants', 'infects', 'inferno', 'infests', 'infidel', 'infield', 'inflame', 'inflate', 'inflect', 'inflict', 'inflows', 'informs', 'infused', 'infuses', 'ingenue', 'ingests', 'ingrain', 'ingrate', 'ingress', 'ingrown', 'inhabit', 'inhaled', 'inhaler', 'inhales', 'inhered', 'inheres', 'inherit', 'inhibit', 'inhumed', 'inhumes', 'inhuman', 'initial', 'injects', 'injured', 'injures', 'inkblot', 'inkhorn', 'inkiest', 'inkling', 'inkwell', 'inmates', 'innards', 'innings', 'inquest', 'inquire', 'inquiry', 'inroads', 'insects', 'inserts', 'inshore', 'insider', 'insides', 'insight', 'insipid', 'insists', 'insofar', 'insoles', 'inspect', 'inspire', 'install', 'instant', 'instate', 'instead', 'insteps', 'instill', 'insular', 'insulin', 'insults', 'insured', 'insurer', 'insures', 'intakes', 'integer', 'intends', 'intense', 'intents', 'interim', 'interns', 'intoned', 'intones', 'introit', 'intrude', 'intuits', 'inulins', 'inuring', 'invaded', 'invader', 'invades', 'invalid', 'inveigh', 'invents', 'inverse', 'inverts', 'invests', 'invited', 'invites', 'invoice', 'invoked', 'invokes', 'involve', 'inwards', 'iodides', 'iodines', 'iodized', 'iodizes', 'ionized', 'ionizes', 'ipecacs', 'iranian', 'ireland', 'iridium', 'irksome', 'ironing', 'ironies', 'irrupts', 'ischium', 'islamic', 'islands', 'isobars', 'isolate', 'isomers', 'isotope', 'israeli', 'issuing', 'isthmus', 'italics', 'italian', 'itching', 'itchier', 'itemize', 'iterate', 'ivories', 'jabbing', 'jabbers', 'jacking', 'jackals', 'jackass', 'jackdaw', 'jackets', 'jackpot', 'jackson', 'jadeite', 'jagging', 'jaguars', 'jailing', 'jailers', 'jakarta', 'jamaica', 'jamming', 'jangles', 'jangled', 'janitor', 'january', 'jarfuls', 'jargons', 'jarring', 'jasmine', 'jaspers', 'jaunted', 'javelin', 'jawbone', 'jaycees', 'jayvees', 'jaywalk', 'jazzing', 'jazzier', 'jealous', 'jeering', 'jehovah', 'jejunum', 'jelling', 'jellies', 'jellied', 'jennies', 'jennets', 'jerboas', 'jerking', 'jerkily', 'jerkier', 'jerkins', 'jerseys', 'jesting', 'jesters', 'jesuits', 'jetport', 'jetting', 'jetties', 'jeweled', 'jeweler', 'jewelry', 'jezebel', 'jibbing', 'jiffies', 'jigging', 'jiggers', 'jiggles', 'jiggled', 'jigsaws', 'jilting', 'jimmied', 'jimmies', 'jingles', 'jingled', 'jingoes', 'jinxing', 'jitneys', 'jitters', 'jittery', 'jobbing', 'jobbers', 'jobbery', 'jobless', 'jockeys', 'jocular', 'jogging', 'joggers', 'joggles', 'joggled', 'joining', 'joiners', 'joinery', 'jointed', 'jointer', 'jointly', 'jollier', 'jollity', 'jolting', 'jonquil', 'joshing', 'jostles', 'jostled', 'jotting', 'jounced', 'jounces', 'journal', 'journey', 'jousted', 'joyless', 'jobilee', 'judaism', 'judases', 'judging', 'juggles', 'juggled', 'juggler', 'jugular', 'juicing', 'juicers', 'juicily', 'juicier', 'jujitsu', 'jujubes', 'jukebox', 'jumbles', 'jumbled', 'jumping', 'jumpers', 'jumpily', 'jumpier', 'jungles', 'juniors', 'juniper', 'junking', 'junkies', 'junkets', 'junkmen', 'junkman', 'jupiter', 'jurists', 'justest', 'justice', 'justify', 'jutting', 'kaolins', 'karakul', 'katydid', 'kayaked', 'keeling', 'keening', 'keenest', 'keeping', 'keepers', 'kelvins', 'kenning', 'kennels', 'kenyans', 'keratin', 'kernels', 'kestrel', 'ketches', 'ketchup', 'kettles', 'keycaps', 'keyhole', 'keynote', 'keypads', 'keyword', 'khedive', 'kibbutz', 'kicking', 'kickers', 'kickoff', 'kidding', 'kiddies', 'kidnaps', 'kidneys', 'kidskin', 'killing', 'killers', 'killjoy', 'kilobit', 'kiloton', 'kilters', 'kimonos', 'kindest', 'kindles', 'kindled', 'kindred', 'kinetic', 'kinfolk', 'kingdom', 'kingpin', 'kinking', 'kinkier', 'kinship', 'kinsmen', 'kinsman', 'kippers', 'kismets', 'kissing', 'kissers', 'kitchen', 'kitties', 'kittens', 'kiwanis', 'kleenex', 'kludged', 'kludges', 'klutzes', 'knavery', 'knavish', 'kneaded', 'kneeing', 'kneecap', 'kneeler', 'kneepad', 'knelled', 'knifing', 'knights', 'knishes', 'knitted', 'knitter', 'knobbed', 'knocked', 'knocker', 'knotted', 'knowing', 'knuckle', 'knurled', 'kookier', 'kopecks', 'koreans', 'kowtows', 'kremlin', 'krishna', 'krypton', 'kummels', 'kumquat', 'labeled', 'labeler', 'labored', 'laborer', 'laciest', 'lacings', 'lacking', 'lackeys', 'laconic', 'lacquer', 'lactary', 'lactase', 'lactate', 'lacteal', 'lactose', 'lacunae', 'ladders', 'ladling', 'ladybug', 'lagging', 'laggard', 'lagoons', 'laissez', 'laities', 'lambdas', 'lambent', 'lamella', 'laments', 'laminas', 'laminae', 'laminar', 'lampoon', 'lamprey', 'lancing', 'lancers', 'lancets', 'landing', 'landaus', 'languid', 'languor', 'lankier', 'lankest', 'lanolin', 'lansing', 'lantern', 'lanyard', 'laotian', 'lapland', 'lapping', 'lappers', 'lappets', 'lapsing', 'lapsers', 'lapwing', 'larceny', 'larches', 'larding', 'larders', 'largest', 'largely', 'largess', 'largish', 'lariats', 'larking', 'larrups', 'lasagna', 'lashing', 'lassies', 'lassoed', 'lassoes', 'lasting', 'latched', 'latches', 'lateens', 'latency', 'lateral', 'latexes', 'lathing', 'lathers', 'lathery', 'latrine', 'lattice', 'latvian', 'lauding', 'laughed', 'launder', 'laundry', 'laurels', 'lawless', 'lawsuit', 'lawyers', 'laxness', 'layaway', 'layered', 'layette', 'layoffs', 'layouts', 'layover', 'laziest', 'leached', 'leaches', 'leading', 'leaders', 'leadoff', 'leafing', 'leafier', 'leaflet', 'leagued', 'leaguer', 'leagues', 'leaking', 'leakier', 'leakage', 'leaning', 'leanest', 'leaping', 'learned', 'leasing', 'leashed', 'leashes', 'leather', 'leaving', 'leavens', 'lebanon', 'lechers', 'lechery', 'lectors', 'lectern', 'lecture', 'ledgers', 'leeched', 'leeches', 'leering', 'leerily', 'leerier', 'leeward', 'leeways', 'lefties', 'leftist', 'legally', 'legates', 'legatee', 'legatos', 'legends', 'legging', 'leggier', 'leghorn', 'legibly', 'legible', 'legions', 'legumes', 'legwork', 'leisure', 'lemming', 'lending', 'lenders', 'lengths', 'lengthy', 'lenient', 'lentils', 'leonine', 'leopard', 'leotard', 'leprosy', 'leprous', 'leptons', 'lesbian', 'lesions', 'lesotho', 'lessors', 'lessees', 'lessens', 'lessons', 'letdown', 'letting', 'letters', 'lettuce', 'levator', 'leveled', 'leveler', 'levered', 'levying', 'lewdest', 'lexical', 'lexicon', 'liaison', 'libbers', 'libeled', 'libeler', 'liberal', 'liberia', 'liberty', 'libidos', 'library', 'librium', 'libyans', 'license', 'lichens', 'licking', 'lifting', 'lifters', 'liftoff', 'lighted', 'lighter', 'lightly', 'lighten', 'lignins', 'lignite', 'lignums', 'ligroin', 'likably', 'likable', 'likened', 'likings', 'lilting', 'limbers', 'limeade', 'limited', 'limning', 'limners', 'limping', 'limpets', 'lincoln', 'lindens', 'lineage', 'linemen', 'lineman', 'lingers', 'lingoes', 'lingual', 'linings', 'linking', 'linkers', 'linkage', 'linnets', 'linseed', 'lintels', 'lioness', 'lionize', 'lipases', 'liquors', 'liquefy', 'liqueur', 'liquids', 'lisping', 'lissome', 'listing', 'listens', 'litchis', 'literal', 'lithest', 'lithium', 'litters', 'littler', 'liturgy', 'livable', 'livened', 'livings', 'lizards', 'loading', 'loaders', 'loafing', 'loafers', 'loamier', 'loaning', 'loathed', 'loathes', 'lobbing', 'lobbies', 'lobbied', 'lobelia', 'lobster', 'locales', 'locally', 'located', 'locates', 'locator', 'locking', 'lockers', 'lockets', 'lockjaw', 'lockout', 'lockups', 'locusts', 'lodging', 'lodgers', 'loesses', 'lofting', 'loftily', 'loftier', 'logbook', 'logging', 'loggers', 'loggias', 'logiest', 'logical', 'logjams', 'loiters', 'lolling', 'longest', 'longbow', 'longing', 'looking', 'lookout', 'looming', 'loonier', 'loonies', 'looping', 'loosing', 'loosest', 'loosely', 'loosens', 'looting', 'looters', 'lopping', 'loppers', 'lording', 'lorries', 'lotions', 'lottery', 'lotuses', 'loudest', 'lounged', 'lounges', 'lousing', 'lousier', 'loutish', 'louvers', 'lovably', 'lovable', 'lowborn', 'lowboys', 'lowbrow', 'lowdown', 'lowered', 'lowlier', 'lowland', 'loyally', 'loyalty', 'lozenge', 'lubbers', 'lucifer', 'lucking', 'luckily', 'luckier', 'lugging', 'luggage', 'lulling', 'lullaby', 'lumbers', 'lumbago', 'lumping', 'lumpier', 'lunatic', 'lunched', 'lunches', 'lunette', 'lunging', 'lupines', 'lupuses', 'lurched', 'lurches', 'lurking', 'lushest', 'lusting', 'lusters', 'lustily', 'lustier', 'lustful', 'lyceums', 'lynched', 'lynches', 'lyrical', 'lysines', 'macadam', 'macaque', 'machete', 'machine', 'macrame', 'macrons', 'maculae', 'macular', 'madcaps', 'madders', 'maddest', 'maddens', 'madeira', 'madison', 'madness', 'madonna', 'maenads', 'maestro', 'mafiosi', 'mafioso', 'magcard', 'magenta', 'maggots', 'magical', 'magmata', 'magnate', 'magnets', 'magneto', 'magnify', 'magnums', 'magpies', 'magueys', 'magyars', 'mahatma', 'mahjong', 'mahouts', 'maidens', 'mailing', 'mailers', 'mailbag', 'mailbox', 'mailmen', 'mailman', 'maiming', 'maintop', 'majesty', 'majored', 'majorca', 'makings', 'malaise', 'malaria', 'malefic', 'malices', 'maligns', 'mallard', 'mallets', 'malleus', 'mallows', 'malmsey', 'maltase', 'malteds', 'maltese', 'maltose', 'mammies', 'mammals', 'mammary', 'mammoth', 'manacle', 'managed', 'manager', 'manages', 'manatee', 'mandate', 'mandrel', 'mangers', 'mangier', 'mangles', 'mangled', 'mangoes', 'manhole', 'manhood', 'manhunt', 'maniacs', 'manikin', 'maniocs', 'maniple', 'mankind', 'manlier', 'manmade', 'manning', 'manners', 'mannish', 'mansion', 'mansard', 'mantles', 'mantels', 'mantled', 'mantras', 'manuals', 'manumit', 'manures', 'maoists', 'mapping', 'marabou', 'maracas', 'marauds', 'marbles', 'marbled', 'marched', 'marcher', 'marches', 'margins', 'marimba', 'mariner', 'marines', 'marinas', 'marital', 'marking', 'markers', 'markets', 'markups', 'marlins', 'marline', 'marmots', 'maroons', 'marquee', 'marquis', 'marring', 'marries', 'married', 'marrows', 'marshes', 'marshal', 'martens', 'martial', 'martian', 'martins', 'martini', 'martyrs', 'marvels', 'marxist', 'marxism', 'marxian', 'mascara', 'mascots', 'mashing', 'mashers', 'mashies', 'masking', 'masonic', 'masonry', 'masques', 'massing', 'massive', 'massage', 'masseur', 'massifs', 'masters', 'mastics', 'mastery', 'mastiff', 'mastoid', 'matador', 'matched', 'matches', 'matinee', 'matrons', 'matting', 'matters', 'mattock', 'matured', 'matures', 'maudlin', 'mauling', 'maunder', 'mawkish', 'maxilla', 'maximal', 'maximum', 'maydays', 'mayhems', 'mayoral', 'maypole', 'mazurka', 'meadows', 'meaning', 'meanest', 'meander', 'measles', 'measure', 'meatier', 'meddles', 'meddled', 'meddler', 'medians', 'mediate', 'medical', 'medicos', 'mediums', 'medleys', 'medulla', 'medusas', 'meekest', 'meeting', 'meetest', 'megabit', 'megaton', 'megrims', 'meioses', 'meiosis', 'meiotic', 'meissen', 'melange', 'melanin', 'melding', 'mellows', 'melodic', 'melting', 'members', 'memento', 'memoirs', 'memphis', 'menaced', 'menaces', 'menages', 'mending', 'menials', 'menisci', 'mentors', 'menthes', 'menthol', 'mention', 'meowing', 'mercers', 'mercies', 'mercury', 'merging', 'mergers', 'merinos', 'merited', 'mermaid', 'merrily', 'merrier', 'mescals', 'meshing', 'messing', 'messily', 'messier', 'message', 'messiah', 'meteors', 'metered', 'methane', 'methods', 'methyls', 'metiers', 'metonym', 'metrify', 'mettles', 'mewling', 'mexican', 'mezuzah', 'miasmic', 'miasmal', 'miasmas', 'microbe', 'microns', 'middies', 'middles', 'middays', 'midgets', 'midland', 'midmost', 'midribs', 'midriff', 'midterm', 'midtown', 'midways', 'midweek', 'midwest', 'midwife', 'midyear', 'miffing', 'mignons', 'migrant', 'migrate', 'mikados', 'mildest', 'mildews', 'mileage', 'milieus', 'militia', 'milking', 'milkier', 'milkmen', 'milkman', 'milksop', 'milling', 'millers', 'million', 'milldam', 'millets', 'milords', 'mimesis', 'mimetic', 'mimicry', 'mimosas', 'minaret', 'mincing', 'mincers', 'minding', 'mindful', 'mineral', 'mingles', 'mingled', 'minibus', 'minicab', 'minimal', 'minimum', 'minions', 'minnows', 'minorca', 'minting', 'mintage', 'minuend', 'minuets', 'minuses', 'minutes', 'minutia', 'miocene', 'miracle', 'mirages', 'mirrors', 'miscall', 'miscast', 'miscued', 'miscues', 'misdeal', 'misdeed', 'miserly', 'misfire', 'misfits', 'mishaps', 'mishear', 'mislaid', 'mislays', 'mislead', 'misname', 'misplay', 'misread', 'misrule', 'missing', 'mission', 'missals', 'missile', 'missive', 'misstep', 'misting', 'mistily', 'mistier', 'mistake', 'mistook', 'mistral', 'misused', 'misuses', 'mitered', 'mitoses', 'mitosis', 'mitotic', 'mittens', 'mitzvah', 'mixable', 'mixture', 'mizzens', 'moaning', 'mobbing', 'mobiles', 'mobster', 'mockery', 'mocking', 'mockups', 'modeled', 'moderns', 'modesty', 'modicum', 'modules', 'modular', 'mohairs', 'moiling', 'moister', 'moisten', 'molding', 'molders', 'moldier', 'molests', 'mollies', 'mollify', 'mollusk', 'molting', 'moments', 'monadic', 'monarch', 'mondays', 'moneyed', 'mongers', 'mongols', 'mongrel', 'moniker', 'monists', 'monitor', 'monkeys', 'monkish', 'monocle', 'monodic', 'monomer', 'monsoon', 'monster', 'montage', 'montane', 'montana', 'monthly', 'mooched', 'moocher', 'mooches', 'moodily', 'moodier', 'mooning', 'moonier', 'moonlit', 'mooring', 'moorage', 'moorish', 'mooting', 'mopping', 'moppets', 'moraine', 'morales', 'morally', 'mordant', 'mordent', 'morgues', 'mormons', 'morning', 'morocco', 'moronic', 'morrows', 'morsels', 'mortals', 'mortars', 'mortify', 'mortise', 'mosaics', 'moseyed', 'moslems', 'mosques', 'mossier', 'mothers', 'motions', 'motives', 'motleys', 'motored', 'mottles', 'mottled', 'mottoes', 'mounded', 'mounted', 'mourned', 'mourner', 'mousing', 'mousers', 'mousier', 'mousses', 'mouthed', 'moutons', 'movably', 'movable', 'mucking', 'mucuses', 'muddles', 'muddied', 'muddier', 'muddies', 'muddily', 'muddled', 'muezzin', 'muffing', 'muffles', 'muffins', 'muffled', 'muffler', 'mugging', 'muggers', 'muggier', 'mukluks', 'mulatto', 'mulched', 'mulches', 'mulcted', 'mulling', 'mullein', 'mullion', 'mumbles', 'mumbled', 'mummies', 'mummers', 'mummery', 'mummify', 'munched', 'munches', 'mundane', 'muppets', 'murders', 'murices', 'murkily', 'murkier', 'murmurs', 'murrain', 'muscles', 'muscled', 'museums', 'mushing', 'mushily', 'mushier', 'musical', 'musings', 'muskegs', 'muskets', 'muskrat', 'muslims', 'muslins', 'mussing', 'mussels', 'musters', 'mustier', 'mustang', 'mustard', 'mutably', 'mutable', 'mutants', 'mutated', 'mutates', 'mutters', 'muttons', 'muumuus', 'muzhiks', 'muzzles', 'muzzled', 'mycelia', 'mycoses', 'mycosis', 'myopias', 'myriads', 'myrtles', 'mystery', 'mystics', 'mystify', 'nabbing', 'nacelle', 'nagging', 'naggers', 'nailing', 'nairobi', 'naively', 'naivete', 'nakedly', 'namable', 'namibia', 'nankeen', 'nannies', 'napalms', 'naphtha', 'napkins', 'napping', 'narrate', 'narrows', 'narwhal', 'nascent', 'nastily', 'nastier', 'nations', 'natives', 'nattily', 'nattier', 'natured', 'natures', 'natural', 'naughts', 'naughty', 'nauseas', 'nauseam', 'nearing', 'nearest', 'neatest', 'nebbish', 'nebulae', 'nebular', 'necking', 'necktie', 'nectars', 'needing', 'needier', 'needful', 'needled', 'needles', 'negated', 'negates', 'neglect', 'negroes', 'negroid', 'neither', 'nektons', 'nelsons', 'nemeses', 'nemesis', 'neonate', 'nephews', 'neptune', 'nerving', 'nervier', 'nervosa', 'nervous', 'nesting', 'nestles', 'nestled', 'netsuke', 'netting', 'nettles', 'nettled', 'network', 'neurons', 'neuters', 'neutral', 'neutron', 'newborn', 'newness', 'newsier', 'newsboy', 'newsmen', 'newsman', 'newtons', 'nexuses', 'niacins', 'niagara', 'nibbles', 'nibbled', 'niching', 'nicking', 'nickels', 'niftier', 'nigeria', 'nightly', 'nimbler', 'ninepin', 'ninnies', 'niobium', 'nipping', 'nippers', 'nippier', 'nipples', 'nirvana', 'nitrous', 'nitrate', 'nitride', 'nitrify', 'nitrite', 'nitwits', 'noblest', 'nodding', 'nodders', 'nodules', 'nodular', 'noggins', 'noising', 'noisily', 'noisier', 'noisome', 'nomadic', 'nominal', 'nominee', 'nonages', 'nonagon', 'noncoms', 'nonplus', 'nonsked', 'nonskid', 'nonstop', 'noodles', 'noodled', 'noonday', 'noosing', 'norfolk', 'normals', 'nosegay', 'noshing', 'nosiest', 'nostril', 'nostrum', 'notably', 'notable', 'notched', 'notches', 'nothing', 'noticed', 'notices', 'notions', 'nougats', 'nourish', 'nouveau', 'novella', 'novelty', 'novenas', 'novices', 'nowhere', 'noxious', 'nozzles', 'nuances', 'nubbles', 'nubbins', 'nuclear', 'nucleic', 'nucleon', 'nucleus', 'nudging', 'nudisms', 'nudists', 'nuggets', 'nullity', 'nullify', 'numbers', 'numbest', 'numbing', 'numeral', 'numeric', 'nuncios', 'nunnery', 'nuptial', 'nursing', 'nursery', 'nurture', 'nutmeat', 'nutmegs', 'nutrias', 'nuttier', 'nuzzles', 'nuzzled', 'nymphet', 'oarlock', 'oarsmen', 'oarsman', 'oatmeal', 'obelisk', 'obesity', 'obeying', 'objects', 'oblasts', 'obliged', 'obliges', 'oblique', 'oblongs', 'obloquy', 'oboists', 'obscene', 'obscure', 'obsequy', 'observe', 'obtains', 'obtrude', 'obverse', 'obverts', 'obviate', 'obvious', 'ocarina', 'occlude', 'occults', 'oceanic', 'ocelots', 'octagon', 'octanes', 'octants', 'octaves', 'octavos', 'october', 'octopus', 'oculist', 'oddball', 'oddment', 'odorous', 'odyssey', 'oedipal', 'oedipus', 'offbeat', 'offends', 'offense', 'offered', 'offhand', 'officer', 'offices', 'officio', 'offings', 'offsets', 'offside', 'oiliest', 'oilskin', 'okaying', 'oldster', 'olefins', 'olivine', 'olympic', 'olympia', 'omelets', 'omicron', 'ominous', 'omitted', 'omnibus', 'onboard', 'oneness', 'onerous', 'oneself', 'ongoing', 'onshore', 'onwards', 'ooziest', 'opacity', 'opaqued', 'opaques', 'opening', 'openers', 'operand', 'operate', 'opiated', 'opiates', 'opining', 'opinion', 'opossum', 'opposed', 'opposes', 'oppress', 'optical', 'optimal', 'optimum', 'options', 'opulent', 'oracles', 'oranges', 'orating', 'oration', 'orators', 'oratory', 'orbited', 'orbital', 'orchard', 'orchids', 'ordains', 'ordeals', 'ordered', 'orderly', 'ordinal', 'ordures', 'oregano', 'organdy', 'organic', 'organza', 'orgasms', 'orients', 'orifice', 'origami', 'origins', 'orioles', 'orisons', 'orleans', 'ormolus', 'orotund', 'orphans', 'oscines', 'osmoses', 'osmosis', 'osmotic', 'ospreys', 'osseous', 'ostrich', 'otology', 'ottoman', 'ourself', 'ousting', 'ousters', 'outages', 'outback', 'outbids', 'outcast', 'outcome', 'outcrop', 'outdate', 'outdoes', 'outdoor', 'outdone', 'outface', 'outfits', 'outflow', 'outgoes', 'outgrew', 'outgrow', 'outings', 'outlast', 'outlaws', 'outlays', 'outlets', 'outline', 'outlive', 'outlook', 'outplay', 'outpost', 'outputs', 'outrage', 'outrank', 'outruns', 'outsell', 'outsets', 'outside', 'outsize', 'outsold', 'outstay', 'outward', 'outwear', 'outwits', 'outwore', 'outwork', 'outworn', 'ovaries', 'ovarian', 'ovation', 'overact', 'overage', 'overall', 'overarm', 'overate', 'overawe', 'overbid', 'overdid', 'overdue', 'overeat', 'overlap', 'overlay', 'overlie', 'overpay', 'overran', 'overrun', 'oversaw', 'oversee', 'overtly', 'overtax', 'overuse', 'oviduct', 'oviform', 'ovulate', 'oxblood', 'oxfords', 'oxidant', 'oxidize', 'oxonian', 'oxtails', 'oxygens', 'oxymora', 'oysters', 'pabulum', 'pacific', 'packing', 'packers', 'package', 'packets', 'padding', 'paddies', 'paddles', 'paddled', 'paddock', 'padlock', 'paellas', 'pageant', 'pageboy', 'pagodas', 'paining', 'painful', 'painted', 'painter', 'pairing', 'paisley', 'pajamas', 'palaces', 'paladin', 'palates', 'palatal', 'palaver', 'palette', 'palfrey', 'palings', 'palling', 'pallors', 'pallets', 'palming', 'palmier', 'palmist', 'palmate', 'palpate', 'palsies', 'palsied', 'palters', 'pampers', 'panacea', 'panache', 'pancake', 'panders', 'pandora', 'paneled', 'panicle', 'panicky', 'panning', 'pannier', 'panoply', 'panpipe', 'pansies', 'panting', 'panties', 'panther', 'papayas', 'papered', 'papilla', 'papists', 'papoose', 'paprika', 'papyrus', 'parable', 'paraded', 'parades', 'paradox', 'paragon', 'parapet', 'parasol', 'parboil', 'parcels', 'parched', 'parches', 'pardons', 'parents', 'pareses', 'paresis', 'paretic', 'parfait', 'pariahs', 'parings', 'parking', 'parkway', 'parlors', 'parlous', 'parlays', 'parleys', 'paroled', 'paroles', 'parolee', 'parotid', 'parquet', 'parried', 'parries', 'parrots', 'parsing', 'parsecs', 'parsley', 'parsnip', 'parsons', 'parting', 'parties', 'partake', 'partial', 'partite', 'partner', 'partook', 'parvenu', 'paschal', 'passing', 'passion', 'passive', 'passage', 'passant', 'passkey', 'pasting', 'pastier', 'pasties', 'pastors', 'pastels', 'pastern', 'pastime', 'pasture', 'patched', 'patches', 'patella', 'patents', 'pathway', 'patient', 'patinas', 'patriot', 'patrols', 'patrons', 'patroon', 'patsies', 'patting', 'patters', 'patties', 'pattern', 'paucity', 'paunchy', 'paupers', 'pausing', 'pawning', 'payable', 'paydays', 'paylord', 'payment', 'payoffs', 'payolas', 'payroll', 'peaches', 'peacock', 'peafowl', 'peahens', 'peaking', 'pealing', 'peanuts', 'pearled', 'peasant', 'peaveys', 'pebbles', 'pebbled', 'peccary', 'pecking', 'pectins', 'pedaled', 'pedants', 'peddles', 'peddled', 'peddler', 'peeking', 'peeling', 'peelers', 'peeping', 'peering', 'peerage', 'peeress', 'peeving', 'peevish', 'peewees', 'pegging', 'pelages', 'pelagic', 'pelican', 'pelisse', 'pellets', 'pelting', 'penance', 'penalty', 'pencils', 'pendant', 'pendent', 'pending', 'penguin', 'penises', 'penning', 'pennies', 'pennant', 'pennons', 'pensive', 'pension', 'pentads', 'pentane', 'penuche', 'penults', 'peonies', 'peonage', 'peoples', 'peopled', 'peplums', 'pepping', 'peppier', 'peppers', 'peppery', 'pepsins', 'peptics', 'peptide', 'peptone', 'percale', 'percent', 'perched', 'perches', 'perfect', 'perfidy', 'perform', 'perfume', 'pergola', 'perhaps', 'perigee', 'periled', 'periods', 'periwig', 'perjure', 'perjury', 'perking', 'perkily', 'perkier', 'permits', 'permute', 'perplex', 'persian', 'persist', 'persons', 'persona', 'pertest', 'pertain', 'perturb', 'perukes', 'perused', 'peruses', 'perusal', 'pervade', 'pervert', 'pesetas', 'peskier', 'pesters', 'pestles', 'pestled', 'petaled', 'petards', 'petcock', 'petered', 'petiole', 'petrels', 'petrify', 'petrols', 'petting', 'pettily', 'pettier', 'pettish', 'petunia', 'pewters', 'peyotes', 'phaeton', 'phalanx', 'phallic', 'phallus', 'phantom', 'pharaoh', 'pharynx', 'phasing', 'phenols', 'phenyls', 'philter', 'phlegms', 'phloems', 'phobias', 'phoebes', 'phoenix', 'phoning', 'phonier', 'phonies', 'phonics', 'phoneme', 'phonons', 'photons', 'phrased', 'phrases', 'phrasal', 'physics', 'pianist', 'piazzas', 'picador', 'piccolo', 'picking', 'pickles', 'pickets', 'pickled', 'pickups', 'picnics', 'picture', 'piddles', 'piddled', 'pidgins', 'piebald', 'piecing', 'pierced', 'pierces', 'pieties', 'pietism', 'piffles', 'piffled', 'pigeons', 'pigging', 'piggish', 'piglets', 'pigment', 'pignuts', 'pigpens', 'pigskin', 'pigtail', 'pileups', 'pilfers', 'pilgrim', 'pilings', 'pilling', 'pillion', 'pillage', 'pillars', 'pillbox', 'pillory', 'pillows', 'piloted', 'pimento', 'pimping', 'pimples', 'pimpled', 'pinatas', 'pinball', 'pincers', 'pinched', 'pinches', 'pinging', 'pinhead', 'pinhole', 'piniest', 'pinions', 'pinking', 'pinkies', 'pinkest', 'pinkeye', 'pinning', 'pinnace', 'pinnate', 'pinwale', 'pinworm', 'pioneer', 'piously', 'pipette', 'pipings', 'pipkins', 'pipping', 'pippins', 'piquing', 'piquant', 'piquets', 'piranha', 'pirated', 'pirates', 'piscine', 'pismire', 'pistils', 'pistols', 'pistons', 'pitapat', 'pitched', 'pitcher', 'pitches', 'piteous', 'pitfall', 'pithier', 'pitiful', 'pitting', 'pitying', 'pivoted', 'pivotal', 'pizzazz', 'placing', 'placers', 'placard', 'placate', 'placebo', 'placket', 'plagued', 'plagues', 'plainer', 'plainly', 'plaints', 'plaited', 'planing', 'planers', 'planate', 'planets', 'planked', 'planned', 'planner', 'planted', 'planter', 'plantar', 'plaques', 'plashed', 'plashes', 'plasmas', 'plaster', 'plastic', 'plastid', 'plating', 'plateau', 'platens', 'platoon', 'platted', 'platter', 'plaudit', 'playing', 'players', 'playboy', 'playful', 'playpen', 'pleaded', 'pleased', 'pleases', 'pleated', 'pledged', 'pledges', 'pledgee', 'plenary', 'pleurae', 'pleural', 'pliably', 'pliable', 'pliancy', 'plicate', 'plights', 'plinths', 'plisses', 'plodded', 'plodder', 'plopped', 'plotted', 'plotter', 'plovers', 'plowing', 'plowmen', 'plowman', 'plucked', 'plugged', 'plugger', 'pluming', 'plumage', 'plumbed', 'plumber', 'plummet', 'plumped', 'plumper', 'plunder', 'plunged', 'plunger', 'plunges', 'plunked', 'plurals', 'plusher', 'plushes', 'pluvial', 'plywood', 'poached', 'poacher', 'poaches', 'pockets', 'podiums', 'poesies', 'poetics', 'poetess', 'pogroms', 'pointer', 'pointed', 'poising', 'poisons', 'pokiest', 'polaris', 'polecat', 'polemic', 'policed', 'polices', 'politer', 'politic', 'polkaed', 'polling', 'pollens', 'pollute', 'polygon', 'polymer', 'pomaded', 'pomades', 'pommels', 'pompano', 'pompoms', 'pompons', 'pompous', 'ponchos', 'ponders', 'pongees', 'poniard', 'pontiff', 'pontoon', 'pooches', 'poodles', 'pooling', 'pooping', 'poorest', 'popcorn', 'popeyed', 'popguns', 'poplars', 'poplins', 'popover', 'popping', 'poppers', 'poppies', 'popular', 'porches', 'porcine', 'porgies', 'porkers', 'porkpie', 'porters', 'portals', 'portage', 'portend', 'portent', 'portico', 'portion', 'portray', 'poseurs', 'posited', 'possess', 'possums', 'posting', 'posters', 'postage', 'postbox', 'postern', 'postmen', 'postman', 'posture', 'postwar', 'potable', 'potency', 'pothers', 'potherb', 'pothole', 'pothook', 'potions', 'potluck', 'potpies', 'potting', 'pottier', 'pottage', 'potters', 'pottery', 'pouched', 'pouches', 'poultry', 'pounced', 'pounces', 'pounded', 'pouring', 'pouting', 'pouters', 'poverty', 'powders', 'powdery', 'powered', 'powwows', 'praecox', 'praetor', 'prairie', 'praised', 'praises', 'praline', 'pranced', 'prances', 'pranked', 'prating', 'prattle', 'praying', 'prayers', 'preachy', 'precede', 'precept', 'precise', 'precook', 'predate', 'predict', 'preened', 'prefabs', 'preface', 'prefect', 'prefers', 'preheat', 'prelacy', 'prelate', 'prelude', 'premeds', 'premier', 'premise', 'premium', 'prepaid', 'prepare', 'prepays', 'prepped', 'preppie', 'prepuce', 'presage', 'present', 'preside', 'presoak', 'presort', 'pressed', 'presser', 'presses', 'prestos', 'presume', 'preteen', 'pretend', 'pretest', 'pretext', 'pretzel', 'prevail', 'prevent', 'preview', 'preying', 'pricing', 'pricked', 'prickly', 'prickle', 'priding', 'priests', 'priming', 'primers', 'primacy', 'primary', 'primate', 'primmer', 'primped', 'princes', 'printed', 'printer', 'prisons', 'privies', 'privacy', 'private', 'privets', 'prizing', 'probing', 'probity', 'probate', 'problem', 'proceed', 'process', 'proctor', 'procure', 'prodded', 'prodder', 'prodigy', 'produce', 'product', 'profane', 'profess', 'proffer', 'profile', 'profits', 'profuse', 'progeny', 'program', 'project', 'prolong', 'promise', 'promote', 'prompts', 'pronged', 'pronoun', 'proofed', 'propane', 'propels', 'prophet', 'propose', 'propped', 'prorate', 'prosing', 'prosaic', 'prosody', 'prosper', 'protean', 'protect', 'protege', 'protein', 'protest', 'protons', 'prouder', 'proudly', 'proving', 'proverb', 'provide', 'proviso', 'provoke', 'provost', 'prowess', 'prowled', 'prowler', 'proxies', 'prudent', 'prudery', 'prudish', 'pruning', 'prussia', 'prussic', 'psalter', 'psyches', 'psychic', 'psychos', 'ptyalin', 'puberty', 'publics', 'publish', 'puckers', 'pudding', 'puddles', 'puddled', 'pudgier', 'pueblos', 'puerile', 'puffing', 'puffers', 'puffier', 'puffery', 'puffins', 'pulling', 'pullets', 'pulleys', 'pullman', 'pullout', 'pulping', 'pulpier', 'pulpits', 'pulsing', 'pulsars', 'pulsate', 'pumices', 'pummels', 'pumping', 'pumpkin', 'punched', 'puncher', 'punches', 'pundits', 'pungent', 'puniest', 'punning', 'punster', 'punting', 'punters', 'pupated', 'pupates', 'puppies', 'puppets', 'purging', 'purisms', 'purists', 'puritan', 'purling', 'purlieu', 'purloin', 'purples', 'purpled', 'purport', 'purpose', 'purring', 'pursing', 'pursers', 'pursued', 'pursuer', 'pursues', 'pursuit', 'purveys', 'purview', 'pushing', 'pushers', 'pushily', 'pushier', 'pushpin', 'pushups', 'pussies', 'pustule', 'putoffs', 'putouts', 'putrefy', 'putting', 'putters', 'putties', 'puttees', 'puttied', 'puzzled', 'puzzler', 'puzzles', 'pygmies', 'pyloric', 'pylorus', 'pyramid', 'pyretic', 'pyrites', 'pythons', 'quacked', 'quadrat', 'quaffed', 'quahogs', 'quailed', 'quaking', 'quakers', 'quality', 'qualify', 'quantum', 'quarrel', 'quarter', 'quartet', 'quartos', 'quasars', 'quashed', 'quashes', 'quavers', 'queened', 'queered', 'queerer', 'queerly', 'quelled', 'queried', 'queries', 'quested', 'queuing', 'quibble', 'quiches', 'quicker', 'quickly', 'quicken', 'quickie', 'quieted', 'quieter', 'quietly', 'quietus', 'quilled', 'quilted', 'quinces', 'quinine', 'quintal', 'quintet', 'quipped', 'quitter', 'quivers', 'quivery', 'quizzed', 'quizzes', 'quoined', 'quondam', 'quonset', 'quorums', 'quoting', 'rabbles', 'rabbets', 'rabbits', 'raccoon', 'racemes', 'raceway', 'raciest', 'racisms', 'racists', 'racking', 'rackets', 'racquet', 'radials', 'radians', 'radiant', 'radiate', 'radical', 'radices', 'radioed', 'raffles', 'raffias', 'raffish', 'raffled', 'rafting', 'rafters', 'raggedy', 'ragging', 'raglans', 'ragouts', 'ragtime', 'ragweed', 'raiding', 'raiders', 'railing', 'railway', 'raiment', 'raining', 'rainier', 'rainbow', 'raising', 'raisers', 'raisins', 'raleigh', 'rallied', 'rallies', 'rambles', 'rambled', 'rambler', 'ramekin', 'ramjets', 'ramming', 'rampage', 'rampant', 'rampart', 'ramrods', 'ranched', 'rancher', 'ranches', 'ranchos', 'rancors', 'ranging', 'rangers', 'rangier', 'rangoon', 'ranking', 'rankest', 'rankles', 'rankled', 'ransack', 'ransoms', 'ranting', 'rapiers', 'rapider', 'rapidly', 'rapines', 'rapists', 'rapping', 'rappels', 'rapport', 'rapture', 'rarebit', 'rascals', 'rashers', 'rashest', 'rasping', 'raspier', 'ratably', 'ratable', 'ratchet', 'ratings', 'rations', 'ratites', 'ratline', 'ratting', 'ratters', 'rattier', 'rattles', 'rattans', 'rattled', 'rattler', 'raucous', 'raunchy', 'ravaged', 'ravages', 'raveled', 'ravened', 'ravines', 'ravings', 'ravioli', 'rawhide', 'razings', 'razzing', 'reached', 'reaches', 'reacted', 'reactor', 'reading', 'readers', 'readily', 'readier', 'readies', 'readied', 'reagent', 'realism', 'reality', 'realign', 'realist', 'realize', 'realtor', 'reaming', 'reamers', 'reaping', 'reapers', 'reapply', 'rearing', 'rearmed', 'reasons', 'rebated', 'rebates', 'rebinds', 'rebirth', 'reboots', 'rebound', 'rebuffs', 'rebuild', 'rebuilt', 'rebuked', 'rebukes', 'rebuses', 'recalls', 'recants', 'recasts', 'receded', 'recedes', 'receipt', 'receive', 'recheck', 'recipes', 'recited', 'recites', 'recital', 'reckons', 'reclaim', 'recline', 'recluse', 'recoils', 'records', 'recount', 'recoups', 'recover', 'recruit', 'rectify', 'rectors', 'rectory', 'rectums', 'recycle', 'redacts', 'redbird', 'redcaps', 'redcoat', 'reddest', 'reddens', 'reddish', 'redeems', 'redhead', 'redneck', 'redoing', 'redoubt', 'redound', 'redress', 'reduced', 'reduces', 'redwing', 'redwood', 'reedier', 'reefing', 'reefers', 'reeking', 'reeling', 'referee', 'refills', 'refined', 'refines', 'refiner', 'reflect', 'reforms', 'refract', 'refrain', 'refresh', 'refuels', 'refuges', 'refugee', 'refunds', 'refused', 'refuses', 'refusal', 'refuted', 'refuter', 'refutes', 'regains', 'regaled', 'regales', 'regally', 'regalia', 'regards', 'regatta', 'regency', 'regents', 'regimes', 'regimen', 'regions', 'regnant', 'regress', 'regrets', 'regroup', 'regular', 'reheard', 'rehears', 'rehouse', 'reigned', 'reining', 'reissue', 'rejects', 'rejoice', 'rejoins', 'relapse', 'relater', 'relates', 'related', 'relaxed', 'relaxes', 'relayed', 'release', 'relents', 'reliant', 'reliefs', 'relieve', 'relined', 'relines', 'relived', 'relives', 'relying', 'remains', 'remakes', 'remands', 'remarks', 'remarry', 'rematch', 'reminds', 'remnant', 'remodel', 'remoras', 'remorse', 'remoter', 'remount', 'removed', 'remover', 'removes', 'removal', 'renamed', 'renames', 'rending', 'renders', 'reneged', 'reneges', 'renewed', 'renewal', 'rennets', 'rennins', 'renowns', 'renting', 'rentals', 'reoccur', 'reopens', 'reorder', 'repairs', 'repasts', 'repeals', 'repeats', 'repents', 'repined', 'repines', 'replace', 'replant', 'replays', 'replete', 'replied', 'replies', 'replica', 'reports', 'reposed', 'reposes', 'reposal', 'repress', 'reprint', 'reprise', 'reproof', 'reprove', 'reptile', 'repulse', 'reputes', 'reputed', 'request', 'requiem', 'require', 'requite', 'rereads', 'reroute', 'resales', 'rescind', 'rescued', 'rescuer', 'rescues', 'resells', 'resents', 'reserve', 'reships', 'resided', 'resides', 'residue', 'resigns', 'resists', 'resolve', 'resorts', 'resound', 'respect', 'respell', 'respire', 'respite', 'respond', 'resting', 'restive', 'restart', 'restate', 'restful', 'restock', 'restore', 'results', 'resumed', 'resumes', 'retable', 'retails', 'retains', 'retakes', 'retaken', 'retards', 'retched', 'retches', 'retells', 'rethink', 'retinal', 'retinas', 'retinue', 'retired', 'retires', 'retiree', 'retools', 'retorts', 'retouch', 'retrace', 'retract', 'retrain', 'retread', 'retreat', 'retries', 'retrial', 'returns', 'retyped', 'retypes', 'reunion', 'reunite', 'revamps', 'reveals', 'reveled', 'reveler', 'revelry', 'revenge', 'revenue', 'revered', 'reveres', 'reverie', 'reverse', 'reverts', 'reviews', 'reviled', 'reviles', 'revised', 'reviser', 'revises', 'revisit', 'revived', 'reviver', 'revives', 'revival', 'revoirs', 'revoked', 'revokes', 'revolts', 'revolve', 'revving', 'rewards', 'rewinds', 'rewired', 'rewires', 'rewords', 'reworks', 'rewound', 'rewrite', 'rewrote', 'rezoned', 'rezones', 'rhenium', 'rhizoid', 'rhizome', 'rhodium', 'rhombic', 'rhombus', 'rhubarb', 'rhyming', 'rhythms', 'rialtos', 'ribbing', 'ribbons', 'riboses', 'richest', 'richter', 'rickets', 'rickety', 'ricotta', 'ridding', 'riddles', 'riddled', 'ridging', 'riffles', 'riffled', 'rifling', 'rifting', 'rigging', 'riggers', 'righted', 'rightly', 'rigidly', 'rigueur', 'rimming', 'ringing', 'ringers', 'ringlet', 'rinsing', 'rioting', 'rioters', 'riotous', 'ripcord', 'ripened', 'riposte', 'ripping', 'rippers', 'ripples', 'rippled', 'ripsaws', 'risibly', 'risible', 'risings', 'risking', 'riskier', 'risotto', 'rissole', 'rituals', 'ritzier', 'rivaled', 'rivalry', 'riveted', 'rivulet', 'roaches', 'roadbed', 'roadway', 'roaming', 'roaring', 'roasted', 'roaster', 'robbing', 'robbers', 'robbery', 'rocking', 'rockers', 'rockier', 'rockets', 'rococos', 'rodents', 'roebuck', 'roguery', 'roguish', 'roiling', 'roister', 'rolling', 'rollers', 'rollick', 'romance', 'romaine', 'romping', 'rompers', 'roofing', 'rookery', 'rookies', 'rooming', 'roomers', 'roomier', 'roomful', 'roosted', 'rooster', 'rooting', 'ropiest', 'roseate', 'rosebud', 'rosette', 'rosiest', 'rosters', 'rostrum', 'rotated', 'rotates', 'rotator', 'rotifer', 'rotting', 'rotunda', 'rouging', 'roughed', 'rougher', 'roughly', 'roughen', 'roulade', 'rounded', 'rounder', 'roundly', 'roundel', 'roundup', 'rousing', 'rousted', 'routing', 'routine', 'rowboat', 'rowdier', 'rowdies', 'royally', 'royalty', 'rubbing', 'rubbles', 'rubbers', 'rubbery', 'rubbish', 'rubdown', 'rubella', 'rubicon', 'rubrics', 'ruching', 'ruction', 'rudders', 'ruddier', 'ruffles', 'ruffian', 'ruffled', 'rugbies', 'ruining', 'ruinous', 'rulings', 'rumania', 'rumbles', 'rumbaed', 'rumbled', 'rummies', 'rummage', 'rumored', 'rumples', 'rumpled', 'runaway', 'runback', 'runlets', 'running', 'runners', 'runnier', 'runnels', 'runtime', 'runways', 'rupture', 'rushing', 'rushers', 'russets', 'russian', 'rusting', 'rustier', 'rustles', 'rustics', 'rustled', 'rustler', 'rutting', 'sachems', 'sachets', 'sacking', 'saddest', 'saddles', 'saddens', 'saddled', 'saddler', 'sadiron', 'sadisms', 'sadists', 'sadness', 'safaris', 'saffron', 'sagging', 'saguaro', 'sailing', 'sailors', 'sainted', 'saintly', 'salaams', 'salable', 'salamis', 'salient', 'salines', 'salivas', 'sallied', 'sallies', 'sallows', 'saloons', 'salting', 'saltier', 'saltbox', 'saltine', 'salukis', 'saluted', 'salutes', 'salving', 'salvers', 'salvage', 'salvias', 'samaras', 'sambaed', 'samisen', 'samoans', 'samovar', 'samoyed', 'sampans', 'samples', 'sampled', 'sampler', 'samurai', 'sanctum', 'sanding', 'sanders', 'sandier', 'sandals', 'sandbag', 'sandbar', 'sandbox', 'sandhog', 'sandlot', 'sandmen', 'sandman', 'sangria', 'sapiens', 'sapient', 'sapling', 'sapping', 'sappier', 'sapwood', 'saracen', 'sarcasm', 'sarcoma', 'sardine', 'sarongs', 'sashing', 'sashays', 'sassing', 'sassily', 'sassier', 'satanic', 'satchel', 'sateens', 'satiate', 'satiety', 'satires', 'satiric', 'satisfy', 'satraps', 'satrapy', 'saucing', 'saucers', 'saucily', 'saucier', 'saunter', 'saurian', 'sausage', 'savable', 'savaged', 'savages', 'savanna', 'savants', 'saviors', 'savings', 'savored', 'savvied', 'savvies', 'sawbuck', 'sawdust', 'sawfish', 'sawmill', 'sawyers', 'saxhorn', 'sayings', 'scabies', 'scabbed', 'scaling', 'scalier', 'scalars', 'scalded', 'scalene', 'scallop', 'scalped', 'scalper', 'scalpel', 'scamped', 'scamper', 'scandal', 'scanned', 'scanner', 'scanted', 'scanter', 'scapula', 'scaring', 'scarier', 'scarabs', 'scarcer', 'scarlet', 'scarred', 'scarves', 'scathed', 'scathes', 'scatted', 'scatter', 'scenery', 'scented', 'scepter', 'schemed', 'schemer', 'schemes', 'scherzo', 'schisms', 'schists', 'schleps', 'schlock', 'schnook', 'scholar', 'schools', 'sciatic', 'science', 'scleras', 'scoffed', 'scolded', 'sconces', 'scooped', 'scooted', 'scooter', 'scoring', 'scoriae', 'scorned', 'scorpio', 'scoters', 'scottie', 'scoured', 'scourge', 'scouted', 'scowled', 'scraggy', 'scraped', 'scraper', 'scrapes', 'scrappy', 'scratch', 'scrawls', 'scrawny', 'screams', 'screech', 'screeds', 'screens', 'screwed', 'scribed', 'scriber', 'scribes', 'scribal', 'scrimps', 'scripts', 'scrolls', 'scrooge', 'scrotal', 'scrotum', 'scrubby', 'scruffs', 'scruffy', 'scrunch', 'scruple', 'scudded', 'scuffed', 'scuffle', 'sculled', 'sculpts', 'scummed', 'scupper', 'scuttle', 'scythed', 'scythes', 'seabees', 'seafood', 'sealing', 'sealers', 'seaming', 'seamier', 'seances', 'seaport', 'searing', 'seasick', 'seaside', 'seasons', 'seating', 'seattle', 'seaward', 'seaways', 'seaweed', 'secants', 'seceded', 'secedes', 'seclude', 'seconds', 'secrecy', 'secrets', 'secrete', 'sectors', 'section', 'secular', 'secured', 'securer', 'secures', 'sedated', 'sedates', 'seduced', 'seducer', 'seduces', 'seeding', 'seedier', 'seekers', 'seeking', 'seeming', 'seeping', 'seepage', 'seesaws', 'seethed', 'seethes', 'segment', 'seining', 'seismic', 'seizing', 'seizure', 'selects', 'selfish', 'selling', 'sellers', 'sellout', 'seltzer', 'selvage', 'seminal', 'seminar', 'semites', 'semitic', 'senates', 'senator', 'sending', 'senders', 'sendoff', 'senegal', 'senesce', 'seniors', 'sensing', 'sensate', 'sensors', 'sensory', 'sensual', 'septics', 'septets', 'sequels', 'sequins', 'sequoia', 'serapes', 'seraphs', 'serener', 'serfdom', 'serials', 'serious', 'sermons', 'serpent', 'serrate', 'serried', 'serving', 'servers', 'servant', 'service', 'servile', 'sesames', 'session', 'sessile', 'sestets', 'setback', 'setoffs', 'setting', 'setters', 'settees', 'settled', 'settler', 'settles', 'seventy', 'seventh', 'severed', 'severer', 'several', 'sewages', 'sexiest', 'sexisms', 'sexists', 'sexless', 'sextant', 'sextets', 'sextons', 'shacked', 'shackle', 'shading', 'shadier', 'shadows', 'shadowy', 'shafted', 'shagged', 'shaking', 'shakers', 'shakily', 'shakier', 'shakeup', 'shallot', 'shallow', 'shaming', 'shamans', 'shamble', 'shammed', 'shampoo', 'shaping', 'shapely', 'shapeup', 'sharing', 'sharper', 'sharply', 'sharpen', 'sharpie', 'shatter', 'shaving', 'shavers', 'shavian', 'sheared', 'sheaths', 'sheathe', 'sheaved', 'sheaves', 'shebang', 'shedder', 'sheered', 'sheerer', 'sheeted', 'shekels', 'shelled', 'shellac', 'shelter', 'sheltie', 'shelved', 'shelves', 'sherbet', 'sheriff', 'shields', 'shifted', 'shilled', 'shimmed', 'shimmer', 'shiners', 'shinier', 'shindig', 'shingle', 'shining', 'shinned', 'shipped', 'shipper', 'shirked', 'shirred', 'shivers', 'shivery', 'shoaled', 'shocked', 'shoeing', 'shofars', 'shoguns', 'shooing', 'shoofly', 'shooter', 'shopped', 'shopper', 'shoring', 'shorans', 'shorted', 'shorter', 'shortly', 'shorten', 'shotgun', 'shouted', 'shouter', 'shoving', 'shovels', 'showing', 'showers', 'showily', 'showier', 'showmen', 'showman', 'showoff', 'shrieks', 'shrifts', 'shrikes', 'shrills', 'shrilly', 'shrimps', 'shrined', 'shrines', 'shrinks', 'shrives', 'shriven', 'shrivel', 'shrouds', 'shrubby', 'shticks', 'shucked', 'shudder', 'shuffle', 'shunned', 'shunner', 'shunted', 'shushed', 'shushes', 'shuteye', 'shutoff', 'shutout', 'shutter', 'shuttle', 'shylock', 'shyness', 'shyster', 'siamese', 'siberia', 'sibling', 'sickest', 'sickles', 'sickbay', 'sickbed', 'sickens', 'sickled', 'sidearm', 'sidecar', 'sidemen', 'sideman', 'sidings', 'sidling', 'sieging', 'siennas', 'sierras', 'siestas', 'sieving', 'sifting', 'sifters', 'sighing', 'sighted', 'signing', 'signers', 'signals', 'signets', 'signify', 'silages', 'silence', 'silents', 'silicas', 'silicon', 'silking', 'silkily', 'silkier', 'sillier', 'sillies', 'silting', 'silvers', 'silvery', 'simians', 'similes', 'similar', 'simmers', 'simpers', 'simples', 'simpler', 'simplex', 'sincere', 'singing', 'singers', 'singles', 'singled', 'sinking', 'sinkers', 'sinning', 'sinners', 'sinuous', 'sinuses', 'siphons', 'sipping', 'sirloin', 'sirocco', 'sissies', 'sisters', 'sitcoms', 'sitting', 'sitters', 'situate', 'sixties', 'sixteen', 'sizably', 'sizable', 'sizings', 'sizzles', 'sizzled', 'skating', 'skaters', 'skeptic', 'sketchy', 'skewing', 'skewers', 'skidded', 'skidoos', 'skilled', 'skillet', 'skimmed', 'skimmer', 'skimped', 'skinned', 'skinner', 'skipped', 'skipper', 'skirled', 'skirted', 'skitter', 'skittle', 'skulked', 'skunked', 'skydive', 'skyjack', 'skylark', 'skyline', 'skyward', 'skyways', 'slabbed', 'slacked', 'slacker', 'slacken', 'slagged', 'slaking', 'slaloms', 'slammed', 'slander', 'slanted', 'slapped', 'slashed', 'slashes', 'slating', 'slather', 'slatted', 'slaving', 'slavers', 'slavery', 'slavish', 'slaying', 'slayers', 'sledded', 'sledged', 'sledges', 'sleeked', 'sleeker', 'sleeper', 'sleeted', 'sleeved', 'sleeves', 'sleighs', 'sleight', 'slender', 'sleuths', 'slicing', 'slicked', 'slicker', 'sliding', 'sliders', 'slights', 'sliming', 'slimier', 'slimmed', 'slimmer', 'slipped', 'slipper', 'slither', 'slitted', 'slivers', 'slobber', 'slogans', 'slogged', 'sloping', 'slopped', 'sloshed', 'sloshes', 'slotted', 'sloughs', 'slovaks', 'slovens', 'slovene', 'slowing', 'slowest', 'sludged', 'sludges', 'slugged', 'slugger', 'sluiced', 'sluices', 'slumber', 'slummed', 'slumped', 'slurped', 'slurred', 'slushed', 'slushes', 'slyness', 'smacker', 'smaller', 'smarted', 'smarter', 'smartly', 'smarten', 'smashed', 'smashes', 'smashup', 'smatter', 'smeared', 'smelled', 'smelted', 'smelter', 'smidgen', 'smiling', 'smirked', 'smiting', 'smitten', 'smocked', 'smoking', 'smokers', 'smokier', 'smolder', 'smother', 'smudged', 'smudges', 'smugger', 'smuggle', 'smutted', 'snaffle', 'snafued', 'snagged', 'snaking', 'snakier', 'snapped', 'snapper', 'snaring', 'snarled', 'sneaked', 'sneaker', 'sneered', 'sneezed', 'sneezes', 'snicker', 'snidest', 'sniffed', 'sniffle', 'snifter', 'snigger', 'sniping', 'snipers', 'snipped', 'snippet', 'snivels', 'snooker', 'snooped', 'snoozed', 'snoozes', 'snoring', 'snorkel', 'snorted', 'snowing', 'snowier', 'snowcap', 'snowmen', 'snowman', 'snubbed', 'snubber', 'snuffed', 'snuffle', 'snuggle', 'snugger', 'soaking', 'soakage', 'soaping', 'soapier', 'soapbox', 'soaring', 'sobbing', 'sobered', 'soberer', 'soberly', 'soccers', 'socials', 'society', 'socking', 'sockets', 'sockeye', 'sodding', 'soddens', 'sodiums', 'softies', 'softest', 'softens', 'soggily', 'soggier', 'soiling', 'soilage', 'soirees', 'sojourn', 'solaced', 'solaces', 'solders', 'soldier', 'solicit', 'solider', 'solidly', 'soloing', 'soloist', 'solomon', 'soluble', 'solutes', 'solving', 'solvent', 'somalia', 'somatic', 'someday', 'somehow', 'someone', 'someway', 'sonatas', 'sonnies', 'sonnets', 'soonest', 'sooting', 'sootier', 'soothed', 'soothes', 'sophist', 'sophism', 'sopping', 'soppier', 'soprano', 'sorcery', 'sorghum', 'sorrily', 'sorrier', 'sorrels', 'sorrows', 'sorting', 'sorters', 'sorties', 'sottish', 'souffle', 'soughed', 'soulful', 'sounded', 'sounder', 'soundly', 'soupier', 'souring', 'sourest', 'sources', 'sousing', 'souther', 'soviets', 'soybean', 'spacing', 'spacers', 'spackle', 'spading', 'spangle', 'spaniel', 'spanish', 'spanked', 'spanker', 'spanned', 'spanner', 'sparest', 'sparing', 'sparked', 'sparkle', 'sparred', 'sparrow', 'sparser', 'spartan', 'spastic', 'spathes', 'spatial', 'spatted', 'spatter', 'spatula', 'spavins', 'spawned', 'spaying', 'speaker', 'speared', 'species', 'special', 'specify', 'specked', 'speckle', 'specter', 'spectra', 'speeded', 'speeder', 'speedup', 'spelled', 'speller', 'spender', 'spewing', 'spheres', 'spicing', 'spicily', 'spicier', 'spicule', 'spiders', 'spidery', 'spigots', 'spiking', 'spikier', 'spilled', 'spinier', 'spinals', 'spinach', 'spindly', 'spindle', 'spinets', 'spinner', 'spirals', 'spirits', 'spiting', 'spitted', 'spittle', 'splashy', 'splayed', 'spleens', 'spliced', 'splices', 'splints', 'splotch', 'splurge', 'spoiled', 'spoking', 'spondee', 'sponged', 'sponger', 'sponges', 'sponsor', 'spoofed', 'spooked', 'spooled', 'spooned', 'sporran', 'sported', 'spotted', 'spotter', 'spouses', 'spousal', 'spouted', 'sprains', 'sprawls', 'sprayed', 'sprayer', 'spreads', 'spriest', 'springs', 'springy', 'sprints', 'sprites', 'sprouts', 'spruced', 'sprucer', 'spruces', 'spuming', 'spumoni', 'spurges', 'spurned', 'spurred', 'spurted', 'sputnik', 'sputter', 'squalor', 'squalid', 'squalls', 'squared', 'squarer', 'squares', 'squashy', 'squawks', 'squawky', 'squeaks', 'squeaky', 'squeals', 'squeeze', 'squelch', 'squints', 'squinty', 'squired', 'squires', 'squirms', 'squirmy', 'squirts', 'squishy', 'stables', 'stabbed', 'stabile', 'stabled', 'stabler', 'stacked', 'stadium', 'staffed', 'staging', 'stagier', 'stagger', 'staider', 'stained', 'staking', 'staling', 'stalest', 'stalked', 'stalker', 'stalled', 'stamens', 'stamina', 'stammer', 'stamped', 'stances', 'standby', 'standee', 'standup', 'stanley', 'stannic', 'stanzas', 'staples', 'stapled', 'stapler', 'staring', 'starchy', 'stardom', 'starker', 'starkly', 'starlet', 'starlit', 'starred', 'started', 'starter', 'startle', 'startup', 'starved', 'starves', 'stashed', 'stashes', 'stating', 'stators', 'statics', 'stately', 'station', 'statues', 'stature', 'statute', 'staunch', 'staving', 'staying', 'stealer', 'stealth', 'steamed', 'steamer', 'steeled', 'steeped', 'steeper', 'steeply', 'steeple', 'steepen', 'steered', 'stellar', 'stemmed', 'stencil', 'stepped', 'stepper', 'steppes', 'stepson', 'stereos', 'sterile', 'sterner', 'sternly', 'sternum', 'steroid', 'sterols', 'stetson', 'stetted', 'stewing', 'steward', 'sticker', 'stickle', 'stickup', 'stifles', 'stiffed', 'stiffer', 'stiffly', 'stiffen', 'stifled', 'stigmas', 'stilled', 'stiller', 'stilted', 'stilton', 'stimuli', 'stinger', 'stinker', 'stinted', 'stipend', 'stipple', 'stirred', 'stirrer', 'stirrup', 'stocked', 'stogies', 'stoical', 'stoking', 'stokers', 'stomach', 'stomata', 'stomped', 'stoning', 'stonily', 'stonier', 'stooges', 'stooped', 'stopgap', 'stopped', 'stopper', 'storing', 'stories', 'storage', 'storied', 'stormed', 'stouter', 'stoutly', 'stowing', 'stowage', 'strafed', 'strafes', 'strains', 'straits', 'strands', 'strange', 'stratum', 'stratus', 'strayed', 'streaks', 'streaky', 'streams', 'streets', 'stretch', 'strewed', 'striate', 'strides', 'striker', 'strikes', 'strings', 'stringy', 'striped', 'striper', 'stripes', 'strives', 'striven', 'strobes', 'stroked', 'strokes', 'strolls', 'strophe', 'strudel', 'stubbed', 'stubble', 'studies', 'studded', 'student', 'studied', 'studios', 'stuffed', 'stumble', 'stumped', 'stunned', 'stunner', 'stunted', 'stupefy', 'stupors', 'stutter', 'styling', 'stylish', 'stylist', 'stylize', 'stymies', 'stymied', 'styptic', 'styrene', 'suavity', 'suavest', 'subbing', 'subdued', 'subdues', 'subhead', 'subject', 'subjoin', 'sublets', 'sublime', 'submits', 'suborns', 'subplot', 'subsets', 'subsidy', 'subside', 'subsist', 'subsoil', 'subsume', 'subtend', 'subtler', 'suburbs', 'subvert', 'subways', 'succeed', 'success', 'succors', 'succoth', 'succumb', 'sucking', 'suckers', 'suckles', 'suckled', 'sucrose', 'suction', 'sudsier', 'suffers', 'suffice', 'suffuse', 'sugared', 'suggest', 'suicide', 'suiting', 'suitors', 'sulfate', 'sulfide', 'sulfite', 'sulfurs', 'sulking', 'sulkily', 'sulkier', 'sullied', 'sullies', 'sultans', 'sultana', 'summing', 'summary', 'summers', 'summery', 'summits', 'summons', 'sunbeam', 'sunburn', 'sunders', 'sundaes', 'sundays', 'sundial', 'sundown', 'sunfish', 'sunning', 'sunnier', 'sunrise', 'sunsets', 'sunspot', 'suntans', 'supping', 'suppers', 'suppler', 'support', 'suppose', 'supreme', 'surcoat', 'surfing', 'surfers', 'surface', 'surfeit', 'surging', 'surgeon', 'surgery', 'surlier', 'surmise', 'surname', 'surpass', 'surplus', 'surreal', 'surreys', 'surveys', 'survive', 'suspect', 'suspend', 'sustain', 'suttees', 'sutured', 'sutures', 'suzette', 'svelter', 'swabbed', 'swaddle', 'swaging', 'swagger', 'swahili', 'swallow', 'swamped', 'swanker', 'swapped', 'swapper', 'swarmed', 'swarthy', 'swashed', 'swashes', 'swathed', 'swathes', 'swatted', 'swatter', 'swaying', 'sweated', 'sweater', 'swedish', 'sweeper', 'sweeter', 'sweetly', 'sweeten', 'swelled', 'swelter', 'swerved', 'swerves', 'swifter', 'swiftly', 'swigged', 'swilled', 'swimmer', 'swindle', 'swinger', 'swinish', 'swiping', 'swirled', 'swished', 'swishes', 'swivels', 'swizzle', 'swollen', 'swooned', 'swooped', 'syllabi', 'symbols', 'symptom', 'synapse', 'syncope', 'syndics', 'synergy', 'synonym', 'syrians', 'syringe', 'systems', 'systole', 'tabards', 'tabasco', 'tabbies', 'tabling', 'tableau', 'tablets', 'tabloid', 'tabooed', 'tabular', 'tacitly', 'tacking', 'tackier', 'tackles', 'tackled', 'tackler', 'tactics', 'tactful', 'tactile', 'tadpole', 'taffies', 'taffeta', 'tagging', 'tailing', 'tailors', 'tainted', 'takeoff', 'takings', 'talcums', 'talents', 'talking', 'talkers', 'talkier', 'talkies', 'tallies', 'tallest', 'tallied', 'tallish', 'tallows', 'tallyho', 'taluses', 'tamales', 'tambour', 'tamping', 'tampers', 'tampons', 'tanager', 'tanbark', 'tandems', 'tangier', 'tangles', 'tangelo', 'tangent', 'tangled', 'tangoed', 'tankers', 'tankard', 'tanning', 'tannest', 'tanners', 'tannery', 'tannins', 'tansies', 'tantrum', 'taoists', 'tapered', 'tapioca', 'tapping', 'tappets', 'taproom', 'taproot', 'tapster', 'tardily', 'tardier', 'targets', 'tariffs', 'tarmacs', 'tarnish', 'tarring', 'tarries', 'tarried', 'tarsier', 'tartans', 'tartest', 'tartars', 'tasking', 'tassels', 'tasting', 'tasters', 'tastily', 'tastier', 'tatting', 'tatters', 'tattles', 'tattled', 'tattler', 'tattoos', 'taunted', 'tautest', 'taverns', 'tawnier', 'taxably', 'taxable', 'taxiing', 'taxicab', 'teacart', 'teacher', 'teaches', 'teacups', 'teaming', 'teapots', 'tearing', 'tearful', 'tearoom', 'teasers', 'teasels', 'teasing', 'tedious', 'tediums', 'teeming', 'teenier', 'teeters', 'teethed', 'teethes', 'teheran', 'tektite', 'telexed', 'telexes', 'tellers', 'telling', 'telstar', 'temples', 'tempers', 'tempera', 'tempest', 'tempore', 'tempted', 'tempter', 'tempura', 'tenably', 'tenable', 'tenancy', 'tenants', 'tending', 'tenders', 'tendons', 'tendril', 'tenfold', 'tenpins', 'tensing', 'tension', 'tensest', 'tensely', 'tensile', 'tenting', 'tenters', 'tenuous', 'tenured', 'tenures', 'tequila', 'terabit', 'terbium', 'terming', 'termini', 'termite', 'ternary', 'terries', 'terrace', 'terrain', 'terrier', 'terrify', 'terrors', 'tersest', 'tersely', 'tertian', 'testing', 'testers', 'testily', 'testier', 'testacy', 'testate', 'testify', 'tetanic', 'tetanal', 'tetanus', 'tethers', 'tetrads', 'teutons', 'textile', 'textual', 'texture', 'thalami', 'thanked', 'thawing', 'theater', 'theatre', 'theisms', 'theists', 'theorem', 'therapy', 'thereat', 'thereby', 'therein', 'thereof', 'thereon', 'thereto', 'thermal', 'thermos', 'thicker', 'thickly', 'thicken', 'thicket', 'thieved', 'thieves', 'thimble', 'thinker', 'thinned', 'thinner', 'thirdly', 'thirsts', 'thirsty', 'thistle', 'thither', 'thonged', 'thorium', 'thought', 'threads', 'thready', 'threats', 'thrifty', 'thrills', 'thrived', 'thrives', 'throats', 'throaty', 'thrombi', 'throned', 'thrones', 'throngs', 'through', 'thrower', 'thrusts', 'thruway', 'thudded', 'thulium', 'thumbed', 'thumped', 'thunder', 'thwacks', 'thwarts', 'thyroid', 'thyself', 'tibetan', 'ticking', 'tickers', 'tickles', 'tickets', 'tickled', 'tickler', 'tidbits', 'tidiest', 'tidings', 'tidying', 'tieback', 'tiering', 'tierces', 'tiffing', 'tiffany', 'tighter', 'tightly', 'tighten', 'tigress', 'tilings', 'tilling', 'tillers', 'tillage', 'tilting', 'timbers', 'timbale', 'timbres', 'timider', 'timidly', 'timings', 'timothy', 'timpani', 'tinders', 'tinfoil', 'tingles', 'tingled', 'tinhorn', 'tiniest', 'tinkers', 'tinkles', 'tinkled', 'tinning', 'tinnier', 'tinsels', 'tinting', 'tintype', 'tinware', 'tinwork', 'tipping', 'tippers', 'tipples', 'tippets', 'tippled', 'tippler', 'tipsily', 'tipsier', 'tipster', 'tiptoes', 'tiptoed', 'tiptops', 'tirades', 'tiredly', 'tisanes', 'tissues', 'titanic', 'tithing', 'titians', 'titling', 'titlark', 'titmice', 'titters', 'tittles', 'titular', 'tizzies', 'toadies', 'toadied', 'toasted', 'toaster', 'tobacco', 'toccata', 'tocsins', 'toddies', 'toddles', 'toddled', 'toddler', 'toehold', 'toenail', 'toffees', 'toffies', 'togging', 'toggles', 'toggled', 'toiling', 'toilets', 'tokened', 'tolling', 'toluene', 'tomboys', 'tomcats', 'tomfool', 'tomtits', 'tonally', 'tongued', 'tongues', 'tonight', 'tonnage', 'tonneau', 'tonsils', 'tonsure', 'tontine', 'tooling', 'toolbox', 'tooting', 'toothed', 'topazes', 'topcoat', 'topiary', 'topical', 'topknot', 'topless', 'topmast', 'topmost', 'topping', 'toppers', 'topples', 'toppled', 'topsail', 'topside', 'topsoil', 'torched', 'torches', 'toreros', 'torment', 'tornado', 'torpors', 'torpedo', 'torques', 'torrent', 'torsion', 'torture', 'tossing', 'tossups', 'totaled', 'totally', 'totemic', 'totting', 'totters', 'tottery', 'toucans', 'touched', 'touches', 'touchup', 'toughed', 'tougher', 'toughen', 'toupees', 'touring', 'tourist', 'tourism', 'tourney', 'tousles', 'tousled', 'touting', 'touters', 'towages', 'towards', 'toweled', 'towered', 'towhees', 'towhead', 'towline', 'townies', 'towpath', 'towrope', 'toxemia', 'toxemic', 'tracing', 'tracers', 'tracery', 'trachea', 'tracked', 'tractor', 'trading', 'traders', 'traduce', 'traffic', 'tragedy', 'trailed', 'trailer', 'trained', 'trainer', 'trainee', 'traipse', 'traitor', 'trammel', 'tramped', 'trample', 'tramway', 'trances', 'transit', 'transom', 'trapeze', 'trapped', 'trapper', 'trashed', 'trashes', 'traumas', 'travail', 'travels', 'travois', 'trawled', 'trawler', 'treacle', 'treadle', 'treason', 'treated', 'trebles', 'trebled', 'treeing', 'trefoil', 'trekked', 'trellis', 'tremors', 'tremble', 'tremens', 'tremolo', 'trended', 'trenton', 'trepans', 'trepang', 'tresses', 'trestle', 'triadic', 'triages', 'tribune', 'tribute', 'triceps', 'tricked', 'trickle', 'tricorn', 'tricots', 'trident', 'trifles', 'trifled', 'trigged', 'trigger', 'trijets', 'trilled', 'trilogy', 'trimmed', 'trimmer', 'trinity', 'trinket', 'triodes', 'triples', 'tripled', 'triplet', 'triplex', 'tripods', 'tripoli', 'tripped', 'trireme', 'trisect', 'tritest', 'tritium', 'tritons', 'triumph', 'trivets', 'trivial', 'troches', 'trochee', 'trodden', 'troikas', 'trojans', 'trolled', 'trolley', 'trollop', 'tromped', 'trooped', 'trooper', 'tropism', 'tropics', 'trotted', 'trotter', 'trounce', 'trouble', 'troughs', 'trouped', 'trouper', 'troupes', 'trouser', 'trowels', 'truancy', 'truants', 'trucked', 'trucker', 'truckle', 'trudged', 'trudges', 'truffle', 'truisms', 'trumpet', 'trundle', 'trussed', 'trusses', 'trusted', 'trustee', 'tryouts', 'tsunami', 'tuatara', 'tubbing', 'tubbier', 'tubules', 'tubular', 'tucking', 'tuckers', 'tuesday', 'tufting', 'tugboat', 'tugging', 'tuggers', 'tuition', 'tumbles', 'tumbled', 'tumbler', 'tumbrel', 'tumeric', 'tummies', 'tumults', 'tumulus', 'tunably', 'tunable', 'tundras', 'tuneful', 'tunisia', 'tunnels', 'tupelos', 'turbans', 'turbine', 'tureens', 'turkeys', 'turkish', 'turmoil', 'turning', 'turners', 'turnips', 'turnkey', 'turnoff', 'turnout', 'turrets', 'turtles', 'tussles', 'tussahs', 'tussled', 'tussock', 'tutored', 'tuxedos', 'twanged', 'tweaked', 'tweeter', 'twelfth', 'twelves', 'twiddle', 'twilled', 'twining', 'twinged', 'twinges', 'twinkle', 'twirled', 'twisted', 'twister', 'twitchy', 'twitted', 'twitter', 'twofold', 'twosome', 'tycoons', 'tympana', 'typeset', 'typhoid', 'typhoon', 'typhous', 'typical', 'typists', 'tyranny', 'tyrants', 'ugandan', 'ugliest', 'ukraine', 'ukulele', 'ulsters', 'ululate', 'ulysses', 'umbered', 'umbrage', 'umlauts', 'umpired', 'umpires', 'umpteen', 'unaided', 'unarmed', 'unasked', 'unawake', 'unaware', 'unbends', 'unbinds', 'unbolts', 'unborns', 'unbosom', 'unbound', 'unbowed', 'uncanny', 'unchain', 'uncials', 'uncivil', 'unclasp', 'unclean', 'unclear', 'uncloak', 'unclose', 'uncoils', 'uncorks', 'uncouth', 'uncover', 'uncross', 'unction', 'undergo', 'undoing', 'undress', 'undying', 'unearth', 'uneaten', 'unequal', 'unfired', 'unfixed', 'unfolds', 'unfound', 'unfrock', 'unfurls', 'unglued', 'unglues', 'ungodly', 'unguent', 'unhands', 'unhandy', 'unhappy', 'unheard', 'unhinge', 'unhitch', 'unhooks', 'unhorse', 'unicorn', 'unified', 'unifier', 'unifies', 'uniform', 'unisons', 'uniting', 'unities', 'unitary', 'unkempt', 'unknown', 'unlaced', 'unlaces', 'unlatch', 'unlearn', 'unleash', 'unlined', 'unloads', 'unlocks', 'unloose', 'unlucky', 'unmakes', 'unmanly', 'unmasks', 'unmixed', 'unmoral', 'unmoved', 'unnamed', 'unnerve', 'unpacks', 'unpaved', 'unplugs', 'unquote', 'unravel', 'unready', 'unreels', 'unrests', 'unriper', 'unrolls', 'unscrew', 'unseals', 'unseats', 'unsexed', 'unsexes', 'unsnaps', 'unsnarl', 'unsound', 'unstops', 'unstuck', 'untamed', 'untried', 'untruth', 'untwine', 'untwist', 'untying', 'unusual', 'unveils', 'unwinds', 'unwound', 'unwraps', 'unyoked', 'unyokes', 'upbeats', 'upbraid', 'updated', 'updates', 'updraft', 'upended', 'upgrade', 'upheave', 'uphills', 'upholds', 'upkeeps', 'uplands', 'uplifts', 'upraise', 'upright', 'upriver', 'uproars', 'uproots', 'upshots', 'upstage', 'upstart', 'upstate', 'upsurge', 'upswing', 'upsweep', 'upswept', 'uptakes', 'uptight', 'uptowns', 'upturns', 'upwards', 'uranium', 'urchins', 'uremias', 'ureters', 'urethra', 'urgency', 'urinals', 'urinary', 'urinate', 'urology', 'uruguay', 'useless', 'ushered', 'usually', 'usurers', 'usuries', 'usurped', 'usurper', 'utensil', 'uterine', 'utility', 'utilize', 'utopias', 'utopian', 'uttered', 'utterly', 'uvulars', 'uxorial', 'vacated', 'vacates', 'vaccine', 'vacuity', 'vacuous', 'vacuole', 'vacuums', 'vaginal', 'vaginas', 'vagrant', 'vaguest', 'vaguely', 'vainest', 'valance', 'valence', 'valency', 'valeted', 'valiant', 'validly', 'valises', 'valleys', 'valuing', 'vamoose', 'vamping', 'vampire', 'vandals', 'vandyke', 'vanilla', 'vanning', 'vantage', 'vapored', 'vaquero', 'variant', 'variety', 'various', 'varlets', 'varmint', 'varnish', 'varsity', 'varying', 'vassals', 'vastest', 'vatican', 'vatting', 'vaulted', 'vaunted', 'vectors', 'vedanta', 'veering', 'vegetal', 'vehicle', 'veiling', 'veining', 'vellums', 'velours', 'velvets', 'velvety', 'vending', 'vendors', 'vendees', 'veneers', 'venires', 'venison', 'venting', 'ventral', 'venture', 'verbals', 'verbena', 'verbose', 'verdant', 'verdict', 'verdure', 'verging', 'vergers', 'vermeil', 'vermont', 'vernier', 'versing', 'version', 'versify', 'vertigo', 'vesicle', 'vespers', 'vessels', 'vesting', 'vestals', 'vestige', 'vesture', 'vetches', 'veteran', 'vetoing', 'vetting', 'viaduct', 'vialing', 'viatica', 'vibrant', 'vibrate', 'vibrato', 'viceroy', 'vicious', 'victims', 'victors', 'victory', 'vicunas', 'vietnam', 'viewing', 'viewers', 'vikings', 'village', 'villain', 'villein', 'vincula', 'vinegar', 'vintage', 'vintner', 'violist', 'violate', 'violent', 'violets', 'violins', 'virgins', 'virgule', 'virtues', 'virtual', 'viruses', 'visaing', 'visages', 'viscera', 'viscose', 'viscous', 'visibly', 'visible', 'visions', 'visited', 'visitor', 'visored', 'visuals', 'vitally', 'vitamin', 'vitiate', 'vitrify', 'vitriol', 'vivider', 'vividly', 'vizards', 'viziers', 'vocalic', 'vocally', 'voicing', 'voiding', 'volcano', 'volleys', 'voltage', 'voltaic', 'volubly', 'voluble', 'volumes', 'volutes', 'vomited', 'voodoos', 'votable', 'vouched', 'voucher', 'vouches', 'voyaged', 'voyager', 'voyages', 'voyeurs', 'vulgate', 'vulpine', 'vulture', 'wackier', 'wadding', 'waddles', 'waddled', 'waffles', 'waffled', 'wafting', 'wagered', 'wagging', 'waggles', 'waggish', 'waggled', 'wailing', 'waiting', 'waiters', 'waiving', 'waivers', 'wakeful', 'wakened', 'walking', 'walkers', 'walkout', 'walkups', 'walling', 'wallaby', 'wallets', 'walleye', 'walloon', 'wallops', 'wallows', 'walnuts', 'waltzed', 'waltzes', 'wampums', 'wanders', 'wangles', 'wangled', 'wannest', 'wanting', 'wantons', 'wapitis', 'warbles', 'warbled', 'warbler', 'warding', 'warders', 'wardens', 'warfare', 'warhead', 'wariest', 'warlike', 'warlock', 'warlord', 'warming', 'warmers', 'warmest', 'warmths', 'warning', 'warping', 'warpath', 'warring', 'warrant', 'warrens', 'warrior', 'warship', 'wartime', 'washing', 'washers', 'washday', 'washout', 'washtub', 'waspish', 'wassail', 'wasting', 'wasters', 'wastage', 'wastrel', 'watched', 'watcher', 'watches', 'watered', 'wattles', 'wattage', 'wattled', 'wavered', 'waviest', 'waxiest', 'waxwing', 'waxwork', 'waybill', 'waylaid', 'waylays', 'wayside', 'wayward', 'weakest', 'weakens', 'wealths', 'wealthy', 'weaning', 'weapons', 'wearing', 'wearily', 'wearier', 'wearies', 'wearied', 'weasels', 'weather', 'weaving', 'weavers', 'webbing', 'webster', 'wedding', 'wedging', 'wedgies', 'wedlock', 'weeding', 'weedier', 'weekday', 'weekend', 'weenies', 'weeping', 'weepier', 'weevils', 'weighed', 'weights', 'weighty', 'weirder', 'weirdly', 'weirdos', 'welcome', 'welding', 'welders', 'welfare', 'welkins', 'welling', 'welshed', 'welshes', 'welting', 'welters', 'wenched', 'wenches', 'wending', 'weskits', 'western', 'wetback', 'wetland', 'wetness', 'wetting', 'wettest', 'whacked', 'whaling', 'whalers', 'whammed', 'wharves', 'whatnot', 'wheaten', 'wheedle', 'wheeled', 'wheezed', 'wheezes', 'whelmed', 'whelped', 'whereas', 'whereat', 'whereby', 'wherein', 'whereof', 'whereon', 'whereto', 'whether', 'whetted', 'whiffed', 'whiling', 'whimper', 'whining', 'whinier', 'whipped', 'whipper', 'whippet', 'whipsaw', 'whirled', 'whirred', 'whisked', 'whisker', 'whiskey', 'whisper', 'whistle', 'whiting', 'whitest', 'whitely', 'whitens', 'whither', 'whitish', 'whitlow', 'whittle', 'whizzed', 'whizzes', 'whoever', 'whooped', 'whoopee', 'whopper', 'whoring', 'whorish', 'wickers', 'wickets', 'wickiup', 'widened', 'widgeon', 'widgets', 'widowed', 'widower', 'wielded', 'wieners', 'wigging', 'wiggles', 'wiggled', 'wigwags', 'wigwams', 'wildest', 'wildcat', 'wiliest', 'willful', 'willing', 'willows', 'willowy', 'wilting', 'wimples', 'wimpled', 'wincing', 'winched', 'winches', 'winding', 'windily', 'windier', 'windbag', 'windows', 'windrow', 'windsor', 'winesap', 'winging', 'winking', 'winkles', 'winning', 'winners', 'winnows', 'winsome', 'winters', 'wiretap', 'wiriest', 'wirings', 'wisdoms', 'wishing', 'wishful', 'wisping', 'wispier', 'wistful', 'witched', 'witches', 'withers', 'without', 'witless', 'witness', 'wittily', 'wittier', 'witting', 'wizards', 'wizened', 'wobbles', 'wobbled', 'wolfing', 'wolfram', 'womanly', 'wombats', 'wonders', 'wooding', 'woodier', 'woodcut', 'woofing', 'woofers', 'woolies', 'woolens', 'woozier', 'wording', 'wordier', 'wordage', 'working', 'workers', 'workbox', 'workday', 'workmen', 'workman', 'workout', 'worldly', 'worming', 'worries', 'worried', 'worrier', 'worsens', 'worship', 'worsted', 'wounded', 'wracked', 'wraiths', 'wrangle', 'wrapped', 'wrapper', 'wreaked', 'wreaths', 'wreathe', 'wrecked', 'wrecker', 'wrested', 'wrestle', 'wriggle', 'wringer', 'wrinkle', 'writing', 'writers', 'writhed', 'writhes', 'written', 'wronged', 'wrongly', 'wrought', 'wyoming', 'xeroxed', 'xeroxes', 'yakking', 'yammers', 'yanking', 'yankees', 'yapping', 'yardage', 'yardarm', 'yarrows', 'yawning', 'yearned', 'yelling', 'yellers', 'yellows', 'yelping', 'yeshiva', 'yessing', 'yiddish', 'yielded', 'yipping', 'yippees', 'yodeled', 'yogurts', 'yorkers', 'younger', 'yttrium', 'yummier', 'zambian', 'zaniest', 'zapping', 'zealand', 'zealots', 'zealous', 'zeniths', 'zephyrs', 'zeroing', 'zestful', 'zigzags', 'zilches', 'zillion', 'zincing', 'zinging', 'zingers', 'zinnias', 'zionist', 'zionism', 'zipping', 'zippers', 'zippier', 'zircons', 'zithers', 'zodiacs', 'zombies', 'zooidal', 'zoology', 'zooming', 'zygotes', 'zygotic', 'zymases', 'zymogen', 'zymurgy', 'zyzzyva', 'aardvark', 'abacuses', 'abalones', 'abandons', 'abashing', 'abatable', 'abattoir', 'abbacies', 'abbesses', 'abdicate', 'abdomens', 'abducted', 'abductor', 'aberrant', 'abetting', 'abettors', 'abeyance', 'abhorred', 'abhorrer', 'abidance', 'abjectly', 'abjuring', 'abjurers', 'ablation', 'ablative', 'ablution', 'abnegate', 'abnormal', 'aborting', 'abortive', 'abortion', 'abounded', 'abrading', 'abraders', 'abrasion', 'abrasive', 'abridged', 'abridger', 'abridges', 'abrogate', 'abruptly', 'abscised', 'abscises', 'abscissa', 'absconds', 'absences', 'absented', 'absently', 'absentee', 'absentia', 'absinthe', 'absolute', 'absolved', 'absolver', 'absolves', 'absorbed', 'absorber', 'abstains', 'abstract', 'abstruse', 'absurdly', 'absurdum', 'abundant', 'abutment', 'abutting', 'abutters', 'academes', 'academia', 'academic', 'acanthus', 'acceding', 'acceders', 'accented', 'accepted', 'acceptor', 'accessed', 'accesses', 'accident', 'acclaims', 'accolade', 'accompli', 'accorded', 'accosted', 'accounts', 'accouter', 'accredit', 'accruing', 'accruals', 'accuracy', 'accurate', 'accursed', 'accusers', 'accusing', 'accustom', 'acerbity', 'acerbate', 'acetates', 'acetones', 'achieved', 'achiever', 'achieves', 'achilles', 'acidosis', 'acolytes', 'aconites', 'acoustic', 'acquaint', 'acquired', 'acquirer', 'acquires', 'acreages', 'acridity', 'acrimony', 'acrobats', 'acronyms', 'acrostic', 'acrylics', 'actinism', 'actinide', 'actinium', 'activist', 'activism', 'activity', 'activate', 'actively', 'actually', 'actuated', 'actuates', 'actuator', 'acuities', 'adapting', 'adapters', 'adaptive', 'addendum', 'addicted', 'addition', 'additive', 'adducing', 'adducers', 'adducted', 'adductor', 'adenines', 'adenoids', 'adequacy', 'adequate', 'adhering', 'adherent', 'adhesion', 'adhesive', 'adjacent', 'adjoined', 'adjourns', 'adjudged', 'adjudges', 'adjuncts', 'adjuring', 'adjurers', 'adjusted', 'adjuster', 'adjutant', 'admirers', 'admirals', 'admiring', 'admitted', 'admixing', 'admonish', 'adopting', 'adopters', 'adoption', 'adoptive', 'adorably', 'adorable', 'adorning', 'adroitly', 'adsorbed', 'adulated', 'adulates', 'adulator', 'adultery', 'advanced', 'advancer', 'advances', 'adverted', 'advising', 'advisers', 'advisors', 'advisory', 'advocacy', 'advocate', 'aerating', 'aeration', 'aerators', 'aerobics', 'aerology', 'aeronaut', 'aerosols', 'aerostat', 'aesthete', 'affected', 'afferent', 'affiance', 'affinity', 'affirmed', 'affixing', 'afflatus', 'afflicts', 'affluent', 'afforded', 'affronts', 'aflutter', 'africans', 'agedness', 'agencies', 'ageratum', 'aggrieve', 'agitates', 'agitator', 'agitated', 'aglimmer', 'aglitter', 'agnostic', 'agonized', 'agonizes', 'agrarian', 'agreeing', 'agronomy', 'aigrette', 'ailerons', 'ailments', 'airborne', 'airbrush', 'airbuses', 'aircraft', 'airdrome', 'airdrops', 'airedale', 'airfield', 'airfoils', 'airglows', 'airiness', 'airlifts', 'airliner', 'airlines', 'airmails', 'airplane', 'airports', 'airships', 'airspace', 'airstrip', 'airtight', 'airwaves', 'alacrity', 'alarmist', 'alarming', 'albacore', 'albinism', 'albumens', 'albumins', 'alcazars', 'alcohols', 'aldehyde', 'aldermen', 'alderman', 'alehouse', 'alembics', 'alerting', 'alewives', 'alfalfas', 'alfresco', 'algebras', 'algerian', 'alibiing', 'aliening', 'alienist', 'alienate', 'alighted', 'aligning', 'aliments', 'aliquant', 'alkaline', 'alkalize', 'alkaloid', 'allaying', 'alleging', 'allegers', 'allegory', 'allegros', 'alleluia', 'allergic', 'allergen', 'alleyway', 'alliance', 'allocate', 'allonyms', 'allotted', 'allowing', 'alloying', 'allspice', 'alluding', 'alluders', 'alluring', 'allusion', 'allusive', 'alluvial', 'alluvium', 'almanacs', 'almighty', 'almoners', 'alphabet', 'altering', 'althorns', 'although', 'altitude', 'altruist', 'altruism', 'aluminas', 'aluminum', 'alveolar', 'alveolus', 'alyssums', 'amalgams', 'amanitas', 'amaranth', 'amassing', 'amateurs', 'ambiance', 'ambience', 'ambition', 'ambrosia', 'ambulant', 'ambulate', 'ambushed', 'ambushes', 'amenably', 'amenable', 'amending', 'amercing', 'americas', 'american', 'amethyst', 'amicably', 'amicable', 'amitoses', 'amitosis', 'amitotic', 'ammeters', 'ammonias', 'ammoniac', 'ammonite', 'ammonium', 'amnesias', 'amnesiac', 'amortize', 'amounted', 'amperage', 'amphorae', 'ampoules', 'amputate', 'amputees', 'amusedly', 'amylases', 'anabolic', 'anaconda', 'anaerobe', 'anagrams', 'analogue', 'analyses', 'analysis', 'analysts', 'analytic', 'analyzed', 'analyzer', 'analyzes', 'anapests', 'anaphase', 'anaphors', 'anaphora', 'anarchic', 'anathema', 'ancestor', 'ancestry', 'anchored', 'ancients', 'andantes', 'andirons', 'androgen', 'androids', 'anecdote', 'anemones', 'aneurysm', 'angelica', 'angering', 'anglican', 'angriest', 'angstrom', 'anilines', 'animisms', 'animates', 'animator', 'animated', 'animists', 'animuses', 'aniseeds', 'anisette', 'annalist', 'annealed', 'annelids', 'annexing', 'annotate', 'announce', 'annoying', 'annually', 'annulled', 'anodized', 'anodizes', 'anodynes', 'anointed', 'anoretic', 'anorexic', 'anorexia', 'answered', 'antacids', 'anteater', 'antedate', 'antelope', 'antennas', 'antennae', 'anterior', 'anteroom', 'anthills', 'antibody', 'antidote', 'antigens', 'antilles', 'antimony', 'antinomy', 'antiphon', 'antiqued', 'antiques', 'antismog', 'antitank', 'antlered', 'antonyms', 'anyplace', 'anything', 'anywhere', 'apathies', 'aperitif', 'aperture', 'aphasics', 'aphasias', 'aphasiac', 'aphelion', 'aphorism', 'apiaries', 'apiarist', 'apologia', 'apoplexy', 'apostles', 'apostasy', 'apostate', 'apothegm', 'apothems', 'appalled', 'appanage', 'apparels', 'apparent', 'appealed', 'appeared', 'appeased', 'appeaser', 'appeases', 'appellee', 'appended', 'appendix', 'appetite', 'applying', 'applauds', 'applause', 'appliers', 'applique', 'appoints', 'apposing', 'apposite', 'appraise', 'apprised', 'apprises', 'approach', 'approved', 'approver', 'approves', 'approval', 'apricots', 'aptitude', 'aquanaut', 'aquarium', 'aquarius', 'aquatint', 'aqueduct', 'aquifers', 'aquiline', 'arabians', 'arachnid', 'arbiters', 'arboreal', 'arcadian', 'archaism', 'archduke', 'archived', 'archival', 'archives', 'archways', 'ardently', 'areaways', 'argosies', 'arguably', 'arguable', 'argument', 'arisings', 'arkansas', 'armament', 'armature', 'armbands', 'armchair', 'armenian', 'armholes', 'armloads', 'armoires', 'armoring', 'armories', 'armorial', 'armrests', 'aromatic', 'arousing', 'arpeggio', 'arraigns', 'arranged', 'arranger', 'arranges', 'arraying', 'arrested', 'arriving', 'arrivals', 'arrogant', 'arrogate', 'arsenals', 'arsenics', 'arsenate', 'arsonist', 'arteries', 'arterial', 'artesian', 'artfully', 'articles', 'articled', 'artifact', 'artifice', 'artisans', 'artistic', 'artistry', 'artworks', 'asbestos', 'ascended', 'ascorbic', 'ascribed', 'ascribes', 'ashtrays', 'asperity', 'aspersed', 'asperses', 'asphalts', 'asphodel', 'asphyxia', 'aspiring', 'aspirant', 'aspirate', 'aspirins', 'assagais', 'assailed', 'assassin', 'assaults', 'assaying', 'assayers', 'assembly', 'assemble', 'assented', 'asserted', 'assessed', 'assesses', 'assessor', 'assigned', 'assignee', 'assisted', 'assonant', 'assorted', 'assuaged', 'assuages', 'assuming', 'assuring', 'astatine', 'asterism', 'asteroid', 'asthenia', 'astonism', 'astounds', 'astutely', 'atavisms', 'ateliers', 'atheisms', 'atheists', 'athenian', 'athletes', 'athletic', 'atlantic', 'atlantis', 'atomized', 'atomizer', 'atomizes', 'atonally', 'atrocity', 'atrophic', 'atropine', 'attached', 'attaches', 'attacked', 'attacker', 'attained', 'attaints', 'attempts', 'attended', 'attested', 'attiring', 'attitude', 'attorney', 'attracts', 'attuning', 'atwitter', 'atypical', 'auctions', 'audacity', 'audience', 'auditing', 'audition', 'auditors', 'auditory', 'augments', 'auguring', 'auguries', 'aureoles', 'auricles', 'auspices', 'austrian', 'authored', 'autistic', 'autobahn', 'autocrat', 'autoharp', 'automats', 'automate', 'autonomy', 'autumnal', 'availing', 'avarices', 'avenging', 'averaged', 'averages', 'averring', 'aversion', 'aversive', 'averting', 'aviaries', 'aviarist', 'aviation', 'aviators', 'aviatrix', 'avionics', 'avocados', 'avoiding', 'avouched', 'avouches', 'avowedly', 'awaiting', 'awakened', 'awarding', 'axletree', 'axolotls', 'azimuths', 'azurites', 'babbling', 'babushka', 'baccarat', 'bachelor', 'bacillus', 'backings', 'backache', 'backbite', 'backbone', 'backdrop', 'backfire', 'backhand', 'backlash', 'backlogs', 'backpack', 'backrest', 'backside', 'backslid', 'backspin', 'backstop', 'backward', 'backwash', 'backyard', 'bacteria', 'badgered', 'badinage', 'badlands', 'badmouth', 'baffling', 'baggiest', 'baggages', 'bagpiper', 'bagpipes', 'baguette', 'bahamian', 'bailiffs', 'bailouts', 'bakelite', 'bakeries', 'baklavas', 'balances', 'balanced', 'baldness', 'baldpate', 'baldrics', 'balinese', 'balkiest', 'balladry', 'ballasts', 'balloons', 'balloted', 'ballroom', 'ballyhoo', 'balmiest', 'baluster', 'banality', 'bandying', 'bandaged', 'bandages', 'bandanna', 'bandeaus', 'bandeaux', 'banditry', 'banished', 'banishes', 'banister', 'banjoist', 'bankbook', 'bankroll', 'bankrupt', 'bannered', 'banquets', 'banshees', 'bantered', 'baptists', 'baptisms', 'baptized', 'baptizes', 'barbered', 'barbados', 'barbaric', 'barbecue', 'barbells', 'barberry', 'barbican', 'barbital', 'bareness', 'bareback', 'barefoot', 'bargains', 'bargello', 'baritone', 'barkings', 'barmaids', 'barnacle', 'barnyard', 'baronies', 'baroness', 'baronets', 'baronial', 'barouche', 'barracks', 'barraged', 'barrages', 'barratry', 'barreled', 'barrette', 'barriers', 'barrooms', 'basaltic', 'basement', 'baseball', 'baseborn', 'baseless', 'basenjis', 'basilica', 'basilisk', 'basketry', 'bassinet', 'bassoons', 'basswood', 'bastions', 'bastings', 'bastards', 'bastille', 'batching', 'bathetic', 'bathoses', 'bathrobe', 'bathroom', 'bathtubs', 'batistes', 'battiest', 'battings', 'battened', 'battered', 'battling', 'bauxites', 'bawdiest', 'bayberry', 'bayonets', 'bazookas', 'beaching', 'beadiest', 'beadwork', 'beanbags', 'beanpole', 'bearably', 'bearable', 'bearings', 'bearding', 'bearskin', 'beatings', 'beatific', 'beatniks', 'beauties', 'beautify', 'becalmed', 'beckoned', 'beclouds', 'becoming', 'bedaubed', 'bedazzle', 'beddings', 'bedecked', 'bedevils', 'bedewing', 'bedizens', 'bedouins', 'bedposts', 'bedrocks', 'bedrolls', 'bedrooms', 'bedsides', 'bedsores', 'bedstead', 'bedtimes', 'beebread', 'beechnut', 'beefiest', 'beehives', 'beelines', 'beetling', 'befallen', 'befitted', 'befogged', 'befouled', 'befriend', 'befuddle', 'begetter', 'beggared', 'beginner', 'begonias', 'begotten', 'begrimed', 'begrimes', 'begrudge', 'beguiles', 'beguiled', 'beguiler', 'beguines', 'behalves', 'behaving', 'behavior', 'beheaded', 'behemoth', 'beholder', 'beholden', 'behooved', 'behooves', 'bejewels', 'belabors', 'belaying', 'belching', 'belfries', 'belgians', 'belgrade', 'believed', 'believer', 'believes', 'belittle', 'bellying', 'bellboys', 'bellhops', 'bellowed', 'belonged', 'beloveds', 'bemiring', 'bemorned', 'bemusing', 'benching', 'benefice', 'benefits', 'benisons', 'benumbed', 'benzenes', 'benzines', 'benzoics', 'benzoate', 'bequeath', 'bequests', 'berating', 'berceuse', 'bereaved', 'bereaves', 'bergamot', 'beriberi', 'bermudas', 'berthing', 'beseemed', 'besieged', 'besieges', 'besmears', 'besmirch', 'besotted', 'besought', 'bespeaks', 'bespoken', 'bespread', 'bestiary', 'bestowed', 'bestowal', 'bestrews', 'bestrewn', 'bestride', 'bestrode', 'betaking', 'betatron', 'bethinks', 'betiding', 'betokens', 'betrayed', 'betrayer', 'betrayal', 'betroths', 'bettered', 'bevatron', 'beveling', 'beverage', 'bewailed', 'bewaring', 'bewilder', 'bezeling', 'beziques', 'biannual', 'biathlon', 'bibelots', 'biblical', 'bibulous', 'bickered', 'bicolors', 'biconvex', 'bicuspid', 'bicycles', 'bicycled', 'biddings', 'biennial', 'bifocals', 'bigamies', 'bigamist', 'bigamous', 'bighorns', 'bikeways', 'billions', 'billings', 'billeted', 'billfold', 'billhook', 'billiard', 'billowed', 'bimetals', 'binaries', 'binaural', 'bindings', 'bindweed', 'binnacle', 'binomial', 'bioassay', 'biologic', 'biopsies', 'bioscopy', 'biplanes', 'biracial', 'birching', 'birdbath', 'birdcall', 'birdlike', 'birdlime', 'birdseed', 'birettas', 'birthing', 'birthday', 'biscuits', 'bisected', 'bisector', 'bisexual', 'bismarck', 'bismuths', 'bitching', 'bitchier', 'bitingly', 'bitterer', 'bitterly', 'bitterns', 'bitumens', 'bivalent', 'bivalves', 'bivouacs', 'biweekly', 'biyearly', 'blabbing', 'blabbers', 'blabbier', 'blacking', 'blackest', 'blackcap', 'blackens', 'blackout', 'blacktop', 'bladders', 'blamably', 'blamable', 'blanched', 'blanches', 'blandest', 'blandish', 'blanking', 'blankest', 'blankets', 'blarneys', 'blasting', 'blastoff', 'blastula', 'blatancy', 'blathers', 'blazoned', 'bleached', 'bleaches', 'bleakest', 'blearing', 'blearier', 'bleating', 'bleeding', 'bleeders', 'bleeping', 'blenched', 'blenches', 'blending', 'blenders', 'blessing', 'blighted', 'blinding', 'blinders', 'blindest', 'blinking', 'blinkers', 'blintzes', 'blipping', 'blissful', 'blisters', 'blithest', 'blithely', 'blitzing', 'blizzard', 'bloating', 'blocking', 'blockade', 'blockage', 'bloodier', 'bloodies', 'bloodied', 'blooming', 'bloomers', 'bloopers', 'blossoms', 'blotched', 'blotches', 'blotting', 'blotters', 'blousing', 'blowfish', 'blowguns', 'blowhard', 'blowhole', 'blowouts', 'blowpipe', 'blowzier', 'blubbers', 'bluchers', 'bludgeon', 'bluebell', 'bluebird', 'bluebook', 'bluefish', 'bluenose', 'bluffing', 'bluffest', 'blunders', 'blunting', 'bluntest', 'blurring', 'blurrier', 'blurting', 'blushing', 'blushers', 'blusters', 'blustery', 'boarding', 'boarders', 'boasting', 'boasters', 'boastful', 'boatload', 'bobbling', 'bobolink', 'bobsleds', 'bobtails', 'bobwhite', 'bodiless', 'bodysurf', 'bogeying', 'boggiest', 'boggling', 'bohemian', 'boldness', 'boldface', 'bolivian', 'bollixed', 'bollixes', 'bolognas', 'bolsters', 'bombings', 'bombards', 'bombasts', 'bonanzas', 'bondages', 'bondsmen', 'bondsman', 'bonefish', 'bonehead', 'boneless', 'bonfires', 'bonhomie', 'bonniest', 'bonneted', 'boodling', 'boohooed', 'bookings', 'bookcase', 'bookends', 'booklets', 'bookmark', 'bookrack', 'bookworm', 'boosting', 'boosters', 'bootjack', 'bootlegs', 'bootless', 'bootlick', 'booziest', 'bordered', 'bordeaux', 'bordello', 'borealis', 'boredoms', 'boroughs', 'borrowed', 'borrower', 'borschts', 'boscages', 'boskiest', 'bosoming', 'bossiest', 'bossisms', 'botanies', 'botanist', 'botching', 'botflies', 'bothered', 'botswana', 'bottling', 'bottomed', 'botulism', 'boudoirs', 'bouffant', 'bouillon', 'boulders', 'bouncing', 'bouncers', 'bouncily', 'bouncier', 'bounding', 'bounders', 'boundary', 'bounties', 'bouquets', 'bourbons', 'boutique', 'bowlines', 'bowsprit', 'boxwoods', 'boycotts', 'boyhoods', 'bracelet', 'brachial', 'brackens', 'brackets', 'brackish', 'bragging', 'braggers', 'braggart', 'brahmans', 'brahmins', 'braiding', 'braining', 'brainier', 'brainpan', 'braising', 'brambles', 'branched', 'branches', 'branding', 'brandies', 'brandied', 'brandish', 'brashest', 'brasilia', 'brassily', 'brassier', 'brattier', 'bravuras', 'brawling', 'brawlers', 'brawnier', 'brazenly', 'braziers', 'breached', 'breaches', 'breadths', 'breaking', 'breakers', 'breakage', 'breakout', 'breakups', 'breasted', 'breathed', 'breather', 'breathes', 'breeches', 'breeding', 'breeders', 'breezing', 'breezily', 'breezier', 'brethren', 'breviary', 'bricking', 'brickbat', 'bridging', 'bridling', 'briefing', 'briefest', 'brigades', 'brigands', 'brighter', 'brightly', 'brighten', 'brimming', 'briniest', 'brindles', 'brindled', 'bringing', 'brioches', 'briskest', 'briskets', 'brisling', 'bristles', 'bristled', 'britches', 'brittles', 'broached', 'broaches', 'broadest', 'broadens', 'broadway', 'brocaded', 'brocades', 'broccoli', 'brochure', 'broiling', 'broilers', 'brokenly', 'bromides', 'bromidic', 'bromines', 'bronchia', 'bronchus', 'bronzing', 'brooches', 'brooding', 'brooders', 'broodier', 'brooking', 'brothels', 'brothers', 'brougham', 'brouhaha', 'browbeat', 'browning', 'brownies', 'brownout', 'browsing', 'bruising', 'bruisers', 'bruiting', 'brunched', 'brunches', 'brunette', 'brushing', 'brusquer', 'brussels', 'brutally', 'bubbling', 'bubblier', 'buckaroo', 'bucketed', 'buckeyes', 'buckling', 'bucklers', 'buckrams', 'bucksaws', 'buckshot', 'buckskin', 'budapest', 'buddhist', 'buddhism', 'budgeted', 'buffered', 'buffeted', 'buffoons', 'bugagoos', 'bugbears', 'building', 'builders', 'bulgiest', 'bulgaria', 'bulkiest', 'bulkhead', 'bullions', 'bullying', 'bulldogs', 'bulldoze', 'bulletin', 'bullfrog', 'bullhead', 'bullocks', 'bullpens', 'bullring', 'bullwhip', 'bulwarks', 'bumbling', 'bumpiest', 'bumpkins', 'bunching', 'bundling', 'bungalow', 'bunghole', 'bungling', 'bunglers', 'buntings', 'buntline', 'buoyancy', 'burbling', 'burdened', 'burdocks', 'burettes', 'burgeons', 'burghers', 'burgling', 'burglars', 'burglary', 'burgundy', 'burliest', 'burnings', 'burnoose', 'burnouts', 'burrowed', 'bursitis', 'bursting', 'bushiest', 'bushings', 'business', 'bustards', 'bustling', 'busyness', 'busybody', 'butchers', 'butchery', 'buttered', 'buttocks', 'buttoned', 'buttress', 'butyrics', 'buzzards', 'bypassed', 'bypasses', 'byssuses', 'cabarets', 'cabbages', 'cabinets', 'cabochon', 'caboodle', 'cabooses', 'cachalot', 'cackling', 'cadavers', 'caddying', 'caddises', 'cadences', 'cadenced', 'cadenzas', 'cadmiums', 'caduceus', 'caesural', 'caesuras', 'caffeine', 'caginess', 'caissons', 'caitiffs', 'cajoling', 'cajolery', 'cakewalk', 'calabash', 'caladium', 'calamity', 'calamine', 'calcined', 'calcines', 'calcites', 'calcitic', 'calciums', 'calculus', 'caldrons', 'calendar', 'calfskin', 'calibers', 'calicoes', 'calipers', 'callings', 'calliope', 'callused', 'calluses', 'calmness', 'calomels', 'calories', 'calorics', 'calumets', 'calypsos', 'cambered', 'cambiums', 'cambodia', 'cambrics', 'camellia', 'cameroon', 'camisole', 'camomile', 'campaign', 'campfire', 'camphors', 'campsite', 'campuses', 'camshaft', 'canadian', 'canaille', 'canalize', 'canaries', 'canastas', 'canberra', 'canceled', 'candying', 'candelas', 'candidly', 'candling', 'canister', 'cankered', 'canniest', 'cannabis', 'cannibal', 'canoeing', 'canoeist', 'canonize', 'canopied', 'canopies', 'cantered', 'cantatas', 'canteens', 'canticle', 'cantinas', 'canvases', 'capacity', 'capering', 'capeskin', 'capitals', 'capitols', 'cappella', 'caprices', 'capriole', 'capsized', 'capsizes', 'capstans', 'capstone', 'capsules', 'capsuled', 'capsular', 'captains', 'captions', 'captious', 'captives', 'captured', 'captures', 'capuchin', 'capybara', 'caracals', 'caracole', 'caramels', 'carapace', 'caravans', 'caravels', 'caraways', 'carbides', 'carbines', 'carbolic', 'carbonic', 'carbonyl', 'carboxyl', 'cardamom', 'cardiacs', 'cardigan', 'cardinal', 'careered', 'careened', 'carefree', 'careless', 'caressed', 'caresses', 'careworn', 'carfares', 'carillon', 'carloads', 'carmines', 'carnages', 'carnival', 'caroling', 'carolers', 'carolina', 'caroming', 'carotene', 'carotids', 'caroused', 'carouses', 'carousal', 'carousel', 'carpeted', 'carports', 'carrions', 'carrying', 'carriers', 'carriage', 'carryall', 'cartages', 'cartoned', 'cartoons', 'carvings', 'caryatid', 'casanova', 'cascaded', 'cascades', 'cascaras', 'casement', 'casework', 'cashiers', 'cashmere', 'cassavas', 'cassette', 'cassocks', 'castings', 'castaway', 'castling', 'castoffs', 'castrate', 'casually', 'casualty', 'casuists', 'catacomb', 'catalogs', 'catalpas', 'catalyst', 'catalyze', 'catapult', 'cataract', 'catarrhs', 'catbirds', 'catboats', 'catcalls', 'catching', 'catchers', 'catchier', 'catchall', 'category', 'catenate', 'catering', 'caterers', 'cathedra', 'catheter', 'cathodes', 'catholic', 'cationic', 'cattiest', 'cattails', 'catwalks', 'caucused', 'caucuses', 'caudates', 'cauldron', 'caulking', 'causally', 'causeway', 'caustics', 'cautious', 'cautions', 'cavalier', 'caviling', 'cavilers', 'cavities', 'cavorted', 'cayennes', 'cecropia', 'cedillas', 'ceilings', 'celebres', 'celeries', 'celerity', 'celestas', 'celibacy', 'celibate', 'cellists', 'cellular', 'cemented', 'cemetery', 'cenobite', 'cenotaph', 'cenozoic', 'censored', 'censured', 'censures', 'censuses', 'centered', 'centaurs', 'centavos', 'centimes', 'centrist', 'centroid', 'cephalic', 'ceramist', 'ceramics', 'cerement', 'cerebral', 'cerebrum', 'ceremony', 'cerulean', 'cervical', 'cervixes', 'cessions', 'cesspool', 'cetacean', 'chaffing', 'chaffers', 'chagrins', 'chaining', 'chairing', 'chairmen', 'chairman', 'chalices', 'chalking', 'chalkier', 'chambers', 'chambray', 'chamfers', 'champing', 'champion', 'chancing', 'chancier', 'chancels', 'chancery', 'chancres', 'chandler', 'changing', 'channels', 'chansons', 'chanting', 'chanters', 'chanteys', 'chanukah', 'chapbook', 'chapeaux', 'chaperon', 'chaplain', 'chaplets', 'chapping', 'chapters', 'chariest', 'charades', 'charcoal', 'charging', 'chargers', 'chariots', 'charisma', 'charleys', 'charmers', 'charming', 'charnels', 'charring', 'charting', 'charters', 'chasings', 'chastity', 'chastest', 'chastens', 'chastise', 'chasuble', 'chateaux', 'chatting', 'chatters', 'chattily', 'chattier', 'chattels', 'cheapest', 'cheapens', 'cheating', 'cheaters', 'checking', 'checkers', 'checksum', 'checkups', 'cheekily', 'cheekier', 'cheeping', 'cheering', 'cheerily', 'cheerier', 'cheerful', 'cheesier', 'cheetahs', 'chemists', 'chemical', 'chemises', 'chemurgy', 'chenille', 'cheroots', 'cherries', 'cherubic', 'cherubim', 'chervils', 'cheshire', 'chessmen', 'chessman', 'chestnut', 'cheviots', 'chevrons', 'chewiest', 'chewable', 'chewinks', 'cheyenne', 'chicanos', 'chickens', 'chickpea', 'chidings', 'chiffons', 'chiggers', 'chignons', 'childish', 'children', 'chileans', 'chilling', 'chillier', 'chimeras', 'chimneys', 'chinches', 'chinking', 'chinning', 'chinooks', 'chintzes', 'chipmunk', 'chipping', 'chippers', 'chirping', 'chirrups', 'chiseled', 'chitchat', 'chitters', 'chivalry', 'chlorals', 'chlorate', 'chloride', 'chlorine', 'chlorite', 'chocking', 'choicest', 'choirboy', 'choleric', 'choleras', 'choosing', 'choosier', 'chopping', 'choppers', 'choppier', 'chorions', 'chorales', 'chordate', 'chorines', 'choroids', 'chortles', 'chortled', 'chorused', 'choruses', 'chowders', 'christen', 'chroming', 'chromate', 'chromite', 'chromium', 'chubbier', 'chucking', 'chuckles', 'chuckled', 'chugging', 'chumming', 'chummier', 'chunking', 'chunkier', 'churches', 'churchly', 'churlish', 'churning', 'chutneys', 'chutzpah', 'cicatrix', 'cicerone', 'ciliates', 'cinching', 'cinchona', 'cincture', 'cinnabar', 'cinnamon', 'ciphered', 'circling', 'circlets', 'circuits', 'circular', 'circuses', 'cirriped', 'cislunar', 'cisterns', 'citadels', 'citation', 'citified', 'citifies', 'citizens', 'citrates', 'citruses', 'citywide', 'civility', 'civilian', 'civilize', 'clabbers', 'clacking', 'cladding', 'claiming', 'claimant', 'clambers', 'clambake', 'clamming', 'clammier', 'clamored', 'clamping', 'clanging', 'clangors', 'clanking', 'clannish', 'clansmen', 'clansman', 'clapping', 'clappers', 'claptrap', 'clarions', 'clarinet', 'clashing', 'clasping', 'classing', 'classier', 'classics', 'classify', 'clatters', 'clavicle', 'claviers', 'cleaning', 'cleaners', 'cleanest', 'cleansed', 'cleanser', 'cleanses', 'cleanups', 'clearing', 'clearest', 'cleaving', 'cleavers', 'cleavage', 'clematis', 'clemency', 'clenched', 'clenches', 'clergies', 'clerical', 'clerking', 'cleverer', 'cleverly', 'clicking', 'clickers', 'climates', 'climatic', 'climaxed', 'climaxes', 'climbing', 'climbers', 'clinched', 'clincher', 'clinches', 'clinging', 'clinical', 'clinking', 'clinkers', 'clipping', 'clippers', 'cliquish', 'clitoral', 'clitoris', 'cloaking', 'clobbers', 'clocking', 'cloddish', 'clogging', 'cloister', 'clopping', 'closings', 'closeted', 'closures', 'clothing', 'clothier', 'clotting', 'clotures', 'clouding', 'cloudily', 'cloudier', 'clouting', 'clowning', 'clubbing', 'clubbier', 'clubfeet', 'clubfoot', 'clucking', 'clumbers', 'clumping', 'clumsily', 'clumsier', 'clusters', 'clutched', 'clutches', 'clutters', 'coaching', 'coachmen', 'coachman', 'coalesce', 'coarsest', 'coarsely', 'coarsens', 'coasting', 'coasters', 'coatings', 'coatroom', 'coattail', 'cobbling', 'cobblers', 'cocaines', 'coccyges', 'cochleae', 'cockiest', 'cockaded', 'cockades', 'cockatoo', 'cockcrow', 'cockerel', 'cockling', 'cockneys', 'cockpits', 'cocksure', 'cocktail', 'coconuts', 'coddling', 'codeines', 'codicils', 'codified', 'codifies', 'codlings', 'coenzyme', 'coequals', 'coercing', 'coercion', 'coercive', 'coexists', 'coextend', 'cogently', 'cogitate', 'cognates', 'cognomen', 'cogwheel', 'cohabits', 'cohering', 'coherent', 'cohesion', 'cohesive', 'coiffeur', 'coiffure', 'coinings', 'coinages', 'coincide', 'coitions', 'coituses', 'colander', 'coldness', 'coleslaw', 'coleuses', 'coliseum', 'collages', 'collagen', 'collapse', 'collared', 'collards', 'collated', 'collates', 'collator', 'collects', 'colleens', 'colleges', 'collided', 'collides', 'colliers', 'colliery', 'colloids', 'colloquy', 'colluded', 'colludes', 'colognes', 'colombia', 'colonies', 'colonist', 'colonels', 'colonial', 'colonize', 'colophon', 'coloring', 'colorist', 'colorado', 'colorant', 'colorful', 'colossal', 'colossus', 'columbia', 'columbic', 'columbus', 'columnar', 'comatose', 'combated', 'combined', 'combines', 'combusts', 'comeback', 'comedies', 'comedian', 'comedown', 'comelier', 'comfiest', 'comforts', 'comities', 'commands', 'commando', 'commedia', 'commence', 'commends', 'comments', 'commerce', 'commixed', 'commixes', 'commodes', 'commoner', 'commonly', 'communed', 'communes', 'communal', 'commuted', 'commuter', 'commutes', 'compacts', 'compadre', 'compared', 'compares', 'compeers', 'competed', 'competes', 'compiled', 'compiler', 'compiles', 'complain', 'complete', 'complied', 'complies', 'complins', 'comports', 'composer', 'composes', 'composed', 'composts', 'compotes', 'compound', 'compress', 'comprise', 'computed', 'computer', 'computes', 'comrades', 'concaves', 'conceals', 'conceded', 'concedes', 'conceits', 'conceive', 'concepts', 'concerns', 'concerts', 'concerto', 'conclave', 'conclude', 'concocts', 'concords', 'concrete', 'condemns', 'condense', 'condigns', 'condoled', 'condoles', 'condoned', 'condones', 'conduced', 'conduces', 'conducts', 'conduits', 'conelrad', 'conferee', 'confetti', 'confided', 'confides', 'confined', 'confines', 'confirms', 'conflict', 'conforms', 'confound', 'confrere', 'confront', 'confuses', 'confused', 'confuted', 'confutes', 'congeals', 'congener', 'congests', 'congress', 'conifers', 'conjoins', 'conjoint', 'conjugal', 'conjunct', 'conjured', 'conjurer', 'conjures', 'connects', 'connived', 'connives', 'connoted', 'connotes', 'conquers', 'conquest', 'consists', 'consents', 'conserve', 'consider', 'consigns', 'consoled', 'consoles', 'consomme', 'consorts', 'conspire', 'constant', 'construe', 'consular', 'consults', 'consumed', 'consumes', 'consumer', 'contacts', 'contains', 'contemns', 'contempt', 'contends', 'contents', 'contests', 'contexts', 'continue', 'continua', 'continuo', 'contorts', 'contours', 'contract', 'contrail', 'contrary', 'contrast', 'contrite', 'contrive', 'controls', 'contused', 'contuses', 'convened', 'convenes', 'convents', 'converge', 'converse', 'converts', 'conveyed', 'conveyer', 'convince', 'convicts', 'convoked', 'convokes', 'convoyed', 'convulse', 'cookbook', 'cookouts', 'coolness', 'coolants', 'coonskin', 'copepods', 'copilots', 'coplanar', 'copperas', 'coppices', 'copulate', 'copyists', 'copybook', 'copycats', 'coquetry', 'coquette', 'coquille', 'coquinas', 'coracles', 'cordages', 'cordials', 'cordites', 'cordless', 'cordoned', 'cordovan', 'corduroy', 'cordwood', 'corkiest', 'corniest', 'cornered', 'corncobs', 'corncrib', 'corneous', 'corniced', 'cornices', 'cornmeal', 'corollas', 'coroners', 'coronary', 'coronets', 'corporal', 'corpuses', 'corrects', 'corridor', 'corroded', 'corrodes', 'corrupts', 'corsages', 'corsairs', 'corseted', 'corsican', 'corteges', 'cortexes', 'cortices', 'cortical', 'corundum', 'corvette', 'cosigned', 'cosigner', 'cosmetic', 'cosmoses', 'cossacks', 'cosseted', 'costlier', 'costumed', 'costumes', 'coteries', 'cottager', 'cottages', 'cottoned', 'couching', 'couchant', 'coughing', 'coulombs', 'councils', 'counsels', 'counting', 'counters', 'counties', 'countess', 'coupling', 'couplers', 'couplets', 'courages', 'couriers', 'coursing', 'coursers', 'courting', 'courtesy', 'courtier', 'coutures', 'covalent', 'covenant', 'covering', 'coverage', 'coverlet', 'covertly', 'coveting', 'covetous', 'cowardly', 'cowbells', 'cowbirds', 'cowering', 'cowgirls', 'cowhands', 'cowherds', 'cowhides', 'cowlings', 'cowlicks', 'coworker', 'cowpokes', 'cowpoxes', 'cowskins', 'cowslips', 'coxcombs', 'coxswain', 'cozening', 'coziness', 'crabbing', 'crabbily', 'crabbier', 'cracking', 'crackers', 'crackles', 'crackled', 'crackpot', 'crackups', 'cradling', 'crafting', 'craftily', 'craftier', 'craggier', 'cramming', 'cramping', 'crampons', 'craniums', 'cranking', 'crankily', 'crankier', 'crannies', 'crappies', 'crashing', 'crassest', 'cravings', 'crawfish', 'crawling', 'crayfish', 'crayoned', 'craziest', 'creaking', 'creakily', 'creakier', 'creaming', 'creamily', 'creamier', 'creamers', 'creamery', 'creasing', 'creating', 'creation', 'creative', 'creators', 'creature', 'credence', 'credenza', 'credibly', 'credible', 'credited', 'creditor', 'creeping', 'creepers', 'creepier', 'cremated', 'cremates', 'cremator', 'creosols', 'creosote', 'crescent', 'cresting', 'cretonne', 'crevasse', 'crevices', 'cribbing', 'cribbage', 'cricking', 'crickets', 'criminal', 'crimping', 'crimsons', 'cringing', 'crinkles', 'crinkled', 'cripples', 'crippled', 'crisping', 'crispers', 'crispier', 'crispest', 'criteria', 'critical', 'critique', 'critters', 'croaking', 'crochets', 'crockery', 'crocuses', 'crofters', 'cromlech', 'crooking', 'crooning', 'crooners', 'cropland', 'cropping', 'croppers', 'croquets', 'crosiers', 'crossing', 'crossbar', 'crossbow', 'crosscut', 'crotchet', 'crouched', 'crouches', 'croupier', 'croutons', 'crowbars', 'crowding', 'crowfoot', 'crowning', 'crucible', 'crucifix', 'cruelest', 'cruising', 'cruisers', 'crullers', 'crumbing', 'crumbles', 'crumbled', 'crummier', 'crumples', 'crumpets', 'crumpled', 'crunched', 'crunches', 'crusaded', 'crusader', 'crusades', 'crushing', 'crusting', 'crustier', 'crutches', 'cryogens', 'cryonics', 'crystals', 'cubicles', 'cuckolds', 'cucumber', 'cuddling', 'cuddlier', 'cudgeled', 'culinary', 'culottes', 'culpably', 'culpable', 'culprits', 'cultists', 'cultured', 'cultures', 'cultural', 'culverts', 'cumbered', 'cumbrous', 'cumulous', 'cunnings', 'cupboard', 'cupcakes', 'cupidity', 'curators', 'curative', 'curdling', 'cureless', 'curettes', 'curfewed', 'curliest', 'curlicue', 'currying', 'currants', 'currency', 'currents', 'cursives', 'curtness', 'curtails', 'curtains', 'curtsied', 'curtsies', 'curvedly', 'cushions', 'cuspidor', 'custards', 'customer', 'cutaways', 'cutbacks', 'cuticles', 'cuttings', 'cutworms', 'cyanides', 'cyanogen', 'cyanoses', 'cyanosis', 'cyanotic', 'cyclists', 'cyclamen', 'cyclamic', 'cyclical', 'cycloids', 'cyclones', 'cyclonic', 'cylinder', 'cynicism', 'cynosure', 'cypriots', 'cyrillic', 'cystitis', 'cytology', 'cytosine', 'czarists', 'czarevna', 'czarinas', 'dabbling', 'dabblers', 'dactylic', 'daedalus', 'daffiest', 'daffodil', 'daintily', 'daintier', 'dainties', 'daiquiri', 'dairying', 'dairymen', 'dairyman', 'dallying', 'damaging', 'damascus', 'damnably', 'damnable', 'dampness', 'dampened', 'dandling', 'dandruff', 'dangling', 'dankness', 'dappling', 'daringly', 'darkness', 'darkened', 'darkroom', 'darlings', 'dartling', 'dashikis', 'database', 'dateline', 'daughter', 'daunting', 'dauphins', 'dawdling', 'daybooks', 'daybreak', 'daydream', 'daylight', 'daystars', 'daytimes', 'dazzling', 'deaconry', 'deadness', 'deadbeat', 'deadened', 'deadhead', 'deadlier', 'deadline', 'deadlock', 'deadpans', 'deadwood', 'deafness', 'deafened', 'dealings', 'deathbed', 'debacles', 'debarked', 'debarred', 'debasing', 'debating', 'debility', 'debiting', 'debonair', 'debriefs', 'debugged', 'debunked', 'debuting', 'decadent', 'decagons', 'decagram', 'decamped', 'decanted', 'decanter', 'decaying', 'deceased', 'deceases', 'decedent', 'deceived', 'deceiver', 'deceives', 'december', 'decently', 'decibels', 'deciding', 'decigram', 'decimals', 'decimate', 'decipher', 'decision', 'decisive', 'deckling', 'declaims', 'declared', 'declares', 'declasse', 'declined', 'declines', 'decoying', 'decocted', 'decoding', 'decoders', 'decorous', 'decorate', 'decorums', 'decrying', 'decrease', 'decrepit', 'decretal', 'decrypts', 'dedicate', 'deducing', 'deducted', 'deepness', 'deepened', 'deerskin', 'defacing', 'defaming', 'defaults', 'defeated', 'defecate', 'defected', 'defector', 'defended', 'defender', 'defenses', 'deferens', 'deferent', 'deferred', 'deferrer', 'deferral', 'defiance', 'deficits', 'defiling', 'defining', 'definers', 'definite', 'deflated', 'deflates', 'deflator', 'deflects', 'deflower', 'deforest', 'deformed', 'defrauds', 'defrayed', 'defrayal', 'defrocks', 'defrosts', 'deftness', 'defusing', 'degraded', 'degrader', 'degrades', 'degrease', 'dehisced', 'dehisces', 'deifying', 'deigning', 'dejected', 'delaware', 'delaying', 'delegate', 'deleting', 'deletion', 'delicacy', 'delicate', 'delights', 'delimits', 'delirium', 'delivers', 'delivery', 'deloused', 'delouses', 'deltoids', 'deluding', 'deluging', 'delusion', 'delusive', 'delusory', 'delvings', 'demagogy', 'demanded', 'demarche', 'demeaned', 'demeanor', 'demented', 'dementia', 'demerits', 'demesnes', 'demigods', 'demijohn', 'demising', 'democrat', 'demolish', 'demoniac', 'demoting', 'demotion', 'demounts', 'demurest', 'demurely', 'demurred', 'demurrer', 'denature', 'dendrite', 'denizens', 'denoting', 'denounce', 'dentists', 'dentines', 'dentinal', 'dentures', 'denuding', 'departed', 'depended', 'depicted', 'deplaned', 'deplanes', 'depleted', 'depletes', 'deplored', 'deplores', 'deployed', 'deponent', 'deported', 'deportee', 'deposing', 'deposals', 'deposits', 'depraved', 'depraves', 'deprived', 'deprives', 'deprival', 'deputing', 'deputies', 'deputize', 'derailed', 'deranged', 'deranges', 'derelict', 'deriding', 'derision', 'derisive', 'deriving', 'derogate', 'derricks', 'derriere', 'desalted', 'descants', 'descends', 'descents', 'descried', 'descries', 'describe', 'deserted', 'deserter', 'deserves', 'deserved', 'designer', 'designee', 'designed', 'desiring', 'desirous', 'desisted', 'desolate', 'despairs', 'despised', 'despises', 'despisal', 'despoils', 'despotic', 'desserts', 'destined', 'destines', 'destroys', 'destruct', 'detached', 'detaches', 'detailed', 'detained', 'detected', 'detector', 'detentes', 'deterred', 'detested', 'dethrone', 'detonate', 'detoured', 'detoxify', 'detracts', 'detrains', 'detritus', 'deuteron', 'devalued', 'devalues', 'develops', 'deviance', 'deviancy', 'deviants', 'deviated', 'deviates', 'deviator', 'deviling', 'devilish', 'deviltry', 'devising', 'devolved', 'devolves', 'devoting', 'devotees', 'devotion', 'devoured', 'devoutly', 'dewberry', 'dewdrops', 'dextrins', 'dextrose', 'diabetic', 'diabolic', 'diagnose', 'diagonal', 'diagrams', 'dialects', 'dialoged', 'dialogue', 'dialyses', 'dialysis', 'dialyzed', 'dialyzes', 'diameter', 'diamonds', 'diapered', 'diapason', 'diarists', 'diarrhea', 'diaspora', 'diastase', 'diastole', 'diatomic', 'diatonic', 'diatribe', 'diazepam', 'dibbling', 'dickered', 'dictated', 'dictates', 'dictator', 'didactic', 'diddling', 'didymium', 'diereses', 'dieresis', 'dietetic', 'differed', 'diffract', 'diffused', 'diffuses', 'digested', 'diggings', 'digitals', 'digitize', 'digraphs', 'dihedral', 'dilantin', 'dilating', 'dilation', 'dilative', 'dilators', 'dilatory', 'dilemmas', 'diligent', 'diluting', 'dilution', 'diluvial', 'diminish', 'dimities', 'dimorphs', 'dimpling', 'dinettes', 'dingiest', 'dingbats', 'dinghies', 'dinkiest', 'dinosaur', 'dioceses', 'diocesan', 'dioramas', 'diorites', 'dioxides', 'diploids', 'diplomas', 'diplomat', 'dipstick', 'diptychs', 'directed', 'directly', 'director', 'dirtiest', 'dirtying', 'disabled', 'disables', 'disabuse', 'disagree', 'disallow', 'disarmed', 'disarray', 'disaster', 'disavows', 'disbands', 'disburse', 'discards', 'discerns', 'disciple', 'disclaim', 'disclose', 'discoids', 'discolor', 'discords', 'discount', 'discover', 'discreet', 'discrete', 'discuses', 'disdains', 'diseased', 'diseases', 'disfavor', 'disgorge', 'disgrace', 'disguise', 'disgusts', 'dishevel', 'dishonor', 'dishpans', 'dishrags', 'disinter', 'disjoins', 'disjoint', 'diskette', 'disliked', 'dislikes', 'dislodge', 'disloyal', 'dismally', 'dismayed', 'dismount', 'disobeys', 'disorder', 'disowned', 'dispatch', 'dispense', 'disperse', 'dispirit', 'displace', 'displays', 'disports', 'disposed', 'disposes', 'disposal', 'disproof', 'disprove', 'disputed', 'disputes', 'disquiet', 'disrobed', 'disrobes', 'disrupts', 'dissects', 'dissents', 'dissever', 'dissolve', 'dissuade', 'distally', 'distance', 'distaffs', 'distaste', 'distends', 'distills', 'distinct', 'distorts', 'distract', 'distress', 'district', 'distrust', 'disturbs', 'disunion', 'disunity', 'disunite', 'disusing', 'ditching', 'dithered', 'diuretic', 'divagate', 'divalent', 'deverged', 'diverges', 'diverted', 'divested', 'dividing', 'dividers', 'dividend', 'divining', 'diviners', 'divinity', 'divinely', 'divisive', 'divisors', 'division', 'divorced', 'divorces', 'divorcee', 'divulged', 'divulger', 'divulges', 'divvying', 'dizzying', 'dizziest', 'doberman', 'docility', 'docilely', 'dockages', 'docketed', 'dockhand', 'dockyard', 'doctored', 'doctoral', 'doctrine', 'document', 'doddered', 'doeskins', 'dogcarts', 'dogfight', 'doggedly', 'doggerel', 'doghouse', 'dogmatic', 'dogtrots', 'dogwatch', 'dogwoods', 'doldrums', 'dolomite', 'dolorous', 'dolphins', 'domestic', 'domicile', 'dominion', 'dominant', 'dominate', 'domineer', 'dominica', 'dominoes', 'donating', 'donation', 'donators', 'doodling', 'doomsday', 'doorbell', 'doorjamb', 'doorknob', 'doormats', 'doornail', 'doorstep', 'doorstop', 'doorways', 'dopiness', 'dormancy', 'dormouse', 'dossiers', 'dotingly', 'dottiest', 'doublest', 'doublets', 'doubloon', 'doubters', 'doubtful', 'doubting', 'douching', 'doughier', 'doughboy', 'doughnut', 'dourness', 'dovecote', 'dovetail', 'dowagers', 'dowdiest', 'doweling', 'dowering', 'downiest', 'downbeat', 'downcast', 'downfall', 'downhill', 'download', 'downpour', 'downtime', 'downtown', 'downturn', 'downward', 'downwind', 'doxology', 'dozenths', 'drabbest', 'dracaena', 'drafting', 'draftier', 'draftees', 'dragging', 'draggers', 'draggles', 'draggled', 'dragnets', 'dragoons', 'draining', 'drainage', 'dramatic', 'dramatis', 'drawings', 'drawback', 'drawling', 'drayages', 'dreading', 'dreadful', 'dreaming', 'dreamers', 'dreamily', 'dreamier', 'drearily', 'drearier', 'dredging', 'drenched', 'drenches', 'dressing', 'dressers', 'dressily', 'dressier', 'dressage', 'dribbles', 'dribbled', 'driblets', 'drifting', 'drifters', 'drilling', 'drinking', 'drinkers', 'dripping', 'drippier', 'driveled', 'driveway', 'drizzles', 'drizzled', 'drollest', 'drollery', 'drooling', 'drooping', 'droopily', 'droopier', 'droplets', 'dropouts', 'dropping', 'droppers', 'dropsies', 'droughts', 'drowning', 'drowsing', 'drowsily', 'drowsier', 'drubbing', 'drudging', 'drudgery', 'drugging', 'druggist', 'drumbeat', 'drumhead', 'drumlins', 'drumming', 'drummers', 'drunkard', 'dualisms', 'duckbill', 'duckling', 'duckpins', 'duckweed', 'dudgeons', 'duelists', 'dukedoms', 'dulcimer', 'dullness', 'dullards', 'dumbness', 'dumbbell', 'dumfound', 'dumpiest', 'dumpling', 'dungeons', 'dunghill', 'duodenal', 'duodenum', 'duperies', 'duplexes', 'durances', 'duration', 'duresses', 'duskiest', 'dustiest', 'dustbins', 'dustpans', 'dutiable', 'dwarfing', 'dwarfism', 'dwelling', 'dwellers', 'dwindles', 'dwindled', 'dyestuff', 'dynamism', 'dynamics', 'dynamist', 'dynamite', 'dynastic', 'dysgenic', 'dyslexic', 'dyslexia', 'eagerest', 'earaches', 'eardrums', 'earflaps', 'earliest', 'earldoms', 'earlobes', 'earmarks', 'earmuffs', 'earnings', 'earnests', 'earphone', 'earrings', 'earshots', 'earthier', 'earwaxes', 'easement', 'easiness', 'easterly', 'eastward', 'eatables', 'eateries', 'ebonites', 'echelons', 'echidnas', 'eclectic', 'eclipsed', 'eclipses', 'ecliptic', 'eclogues', 'ecocides', 'economic', 'ecotypes', 'ecstatic', 'ectoderm', 'edentate', 'edgewise', 'edginess', 'edifying', 'edifices', 'editions', 'educable', 'educated', 'educates', 'educator', 'educible', 'effacing', 'effected', 'efferent', 'efficacy', 'effigies', 'effluent', 'effluvia', 'effluxes', 'effusing', 'effusion', 'effusive', 'eggheads', 'eggplant', 'eggshell', 'egoistic', 'egotists', 'egotisms', 'egressed', 'egresses', 'egyptian', 'eighties', 'eighteen', 'ejecting', 'ejection', 'ejectors', 'elapsing', 'elastics', 'elations', 'elbowing', 'electing', 'election', 'elective', 'electors', 'electric', 'electron', 'elegance', 'elegized', 'elegizes', 'elements', 'elephant', 'elevated', 'elevates', 'elevator', 'eleventh', 'elicited', 'elicitor', 'eligibly', 'eligible', 'elisions', 'elitists', 'elitisms', 'elkhound', 'ellipses', 'ellipsis', 'elliptic', 'elongate', 'eloquent', 'elusions', 'emaciate', 'emanated', 'emanates', 'embalmed', 'embalmer', 'embanked', 'embarked', 'embattle', 'embedded', 'embezzle', 'embitter', 'emblazon', 'embodied', 'embodies', 'embolism', 'embolden', 'embosoms', 'embossed', 'embosses', 'embowers', 'embraced', 'embraces', 'embroils', 'emceeing', 'emending', 'emeralds', 'emerging', 'emergent', 'emeritus', 'emigrant', 'emigrate', 'eminence', 'eminency', 'emirates', 'emission', 'emissive', 'emissary', 'emitting', 'emitters', 'emotions', 'empathic', 'emperors', 'emphases', 'emphasis', 'emphatic', 'employed', 'employer', 'employee', 'emporium', 'empowers', 'emptying', 'emptiers', 'emptiest', 'empyrean', 'emulated', 'emulates', 'emulator', 'emulsion', 'emulsify', 'enabling', 'enacting', 'enactors', 'enameled', 'enamored', 'encamped', 'encasing', 'enchains', 'enchants', 'encipher', 'encircle', 'enclaves', 'enclitic', 'enclosed', 'encloses', 'encoding', 'encoders', 'encomium', 'encoring', 'encroach', 'encrusts', 'encrypts', 'encumber', 'endanger', 'endeared', 'endeavor', 'endemics', 'endocarp', 'endoderm', 'endorsed', 'endorser', 'endorses', 'endorsee', 'endowing', 'endplate', 'endpoint', 'enduring', 'energies', 'energize', 'enervate', 'enfeeble', 'enfilade', 'enfolded', 'enforced', 'enforcer', 'enforces', 'engaging', 'engender', 'engineer', 'engorged', 'engorges', 'engrafts', 'engraved', 'engraver', 'engraves', 'engulfed', 'enhanced', 'enhances', 'enjoined', 'enjoying', 'enkindle', 'enlacing', 'enlarged', 'enlarger', 'enlarges', 'enlisted', 'enlivens', 'enmeshed', 'enmeshes', 'enmities', 'ennobled', 'ennobles', 'enormity', 'enormous', 'enplaned', 'enplanes', 'enraging', 'enriched', 'enriches', 'enrolled', 'ensconce', 'ensemble', 'enshrine', 'enshroud', 'ensiling', 'ensilage', 'enslaved', 'enslaves', 'ensnared', 'ensnares', 'ensuring', 'entailed', 'entangle', 'ententes', 'entering', 'enthrall', 'enthrone', 'enthused', 'enthuses', 'enticing', 'entirely', 'entirety', 'entities', 'entitled', 'entitles', 'entombed', 'entrance', 'entrails', 'entrains', 'entrants', 'entreats', 'entreaty', 'entrench', 'entrusts', 'entryway', 'entwined', 'entwines', 'enuresis', 'envelops', 'envelope', 'envenoms', 'enviably', 'enviable', 'environs', 'envisage', 'envision', 'eohippus', 'eolithic', 'epaulets', 'ephemera', 'epicures', 'epidemic', 'epigrams', 'epigraph', 'epilepsy', 'epilogue', 'epiphany', 'epiphyte', 'episodes', 'episodic', 'epistles', 'epitaphs', 'epithets', 'epsilons', 'equaling', 'equality', 'equalize', 'equating', 'equation', 'equators', 'equipage', 'equipped', 'equities', 'erasable', 'erasures', 'erecting', 'erection', 'erectors', 'eremites', 'erosions', 'ersatzes', 'eructing', 'erupting', 'eruption', 'eruptive', 'escalate', 'escaping', 'escapist', 'escapism', 'escapade', 'escapees', 'escargot', 'escarole', 'eschewed', 'escorted', 'esophagi', 'esoteric', 'espalier', 'espartos', 'especial', 'espoused', 'espouses', 'espousal', 'espresso', 'esquires', 'essaying', 'essences', 'esteemed', 'esthetes', 'esthetic', 'estimate', 'estonian', 'estrange', 'estrogen', 'estruses', 'etageres', 'etchings', 'eternity', 'ethanols', 'ethereal', 'ethiopia', 'ethylene', 'etiolate', 'etiology', 'etruscan', 'eugenics', 'eulogies', 'eulogize', 'euphonic', 'euphoria', 'euphoric', 'european', 'europium', 'evacuate', 'evacuees', 'evaluate', 'evanesce', 'evasions', 'evenings', 'eventful', 'eventide', 'eventual', 'evermore', 'everyday', 'everyone', 'evicting', 'eviction', 'evictors', 'evidence', 'evildoer', 'evincing', 'evolving', 'exacting', 'exaction', 'exactors', 'exalting', 'examined', 'examiner', 'examines', 'examples', 'excavate', 'exceeded', 'excelled', 'excepted', 'excerpts', 'excesses', 'exchange', 'excising', 'excision', 'exciting', 'exclaims', 'exclaves', 'excluded', 'excludes', 'excreted', 'excretes', 'excretal', 'excurses', 'excursus', 'excusing', 'execrate', 'executed', 'executes', 'executor', 'exegeses', 'exegesis', 'exegetic', 'exemplar', 'exemplum', 'exempted', 'exercise', 'exerting', 'exertion', 'exhaling', 'exhausts', 'exhibits', 'exhorted', 'exhuming', 'exigency', 'exiguous', 'existing', 'existent', 'exocrine', 'exoduses', 'exorcist', 'exorcism', 'exorcise', 'expanded', 'expander', 'expanses', 'expected', 'expedite', 'expelled', 'expended', 'expensed', 'expenses', 'expertly', 'expiated', 'expiates', 'expiator', 'expiring', 'explains', 'explicit', 'exploded', 'explodes', 'exploits', 'explored', 'explorer', 'explores', 'exponent', 'exported', 'exporter', 'exposing', 'exposure', 'expounds', 'expunged', 'expunges', 'extended', 'extensor', 'exterior', 'external', 'extolled', 'extoller', 'extorted', 'extracts', 'extremes', 'extruded', 'extrudes', 'exulting', 'exultant', 'eyeballs', 'eyebolts', 'eyebrows', 'eyeglass', 'eyepiece', 'eyeshots', 'eyesight', 'eyesores', 'eyespots', 'eyestalk', 'eyeteeth', 'eyetooth', 'fabulist', 'fabulous', 'faceless', 'faceting', 'facility', 'factions', 'factious', 'factored', 'factotum', 'faddists', 'fagoting', 'faiences', 'failings', 'failures', 'faineant', 'fainting', 'faintest', 'fairness', 'fairways', 'faithful', 'fakeries', 'falconed', 'falconer', 'falconry', 'falkland', 'fallback', 'fallibly', 'fallible', 'fallowed', 'falsetto', 'faltered', 'families', 'familial', 'familiar', 'famished', 'famously', 'fanatics', 'fancying', 'fanciers', 'fanciest', 'fanciful', 'fandango', 'fanfared', 'fanfares', 'fanlight', 'fantails', 'fantasia', 'faradays', 'farceurs', 'farcical', 'farewell', 'farmland', 'farmyard', 'farrowed', 'farthing', 'farthest', 'fascists', 'fascisms', 'fascicle', 'fashions', 'fastness', 'fastback', 'fastened', 'fastener', 'fatalism', 'fatality', 'fatalist', 'fatbacks', 'fatheads', 'fathered', 'fatherly', 'fathomed', 'fatigued', 'fatigues', 'fattiest', 'fattened', 'faulting', 'faultier', 'faustian', 'fauvisms', 'favoring', 'favorite', 'fealties', 'fearless', 'fearsome', 'feasibly', 'feasible', 'feasting', 'feathers', 'feathery', 'featured', 'features', 'february', 'feckless', 'federals', 'federate', 'feeblest', 'feedings', 'feedback', 'feedbags', 'feelings', 'feigning', 'feinting', 'feistier', 'feldspar', 'felicity', 'felonies', 'feltings', 'feminist', 'feminism', 'feminine', 'fencings', 'ferments', 'ferocity', 'ferrying', 'ferreted', 'ferrites', 'ferrules', 'ferruled', 'feruling', 'fervency', 'festered', 'festival', 'festoons', 'fetching', 'fetishes', 'fetlocks', 'fettered', 'fettling', 'feverish', 'fiancees', 'fiascoes', 'fibroids', 'fibroses', 'fibrosis', 'ficklest', 'fictions', 'fiddling', 'fiddlers', 'fidelity', 'fidgeted', 'fielding', 'fielders', 'fiendish', 'fieriest', 'fiercest', 'fiercely', 'fifteens', 'fiftieth', 'fighting', 'fighters', 'figments', 'figuring', 'figurine', 'filament', 'filberts', 'filching', 'filename', 'fileting', 'filigree', 'filipino', 'fillings', 'filleted', 'filliped', 'filmiest', 'filmgoer', 'filtered', 'filthily', 'filthier', 'filtrate', 'finances', 'finagles', 'finagled', 'finalist', 'finality', 'finalize', 'financed', 'findings', 'fineness', 'fineries', 'finessed', 'finesses', 'fingered', 'finished', 'finishes', 'finniest', 'firearms', 'fireball', 'fireboat', 'firebugs', 'firedamp', 'firedogs', 'fireplug', 'fireside', 'firetrap', 'fireweed', 'firewood', 'firmness', 'firmware', 'fishiest', 'fishbowl', 'fisheyes', 'fishhook', 'fishmeal', 'fishnets', 'fishpond', 'fishtail', 'fishwife', 'fissions', 'fissured', 'fissures', 'fistfuls', 'fistulas', 'fitfully', 'fittings', 'fivefold', 'fixating', 'fixation', 'fixative', 'fixtures', 'fizzling', 'flabbily', 'flabbier', 'flagella', 'flagging', 'flagpole', 'flagrant', 'flagship', 'flailing', 'flakiest', 'flamenco', 'flameout', 'flamingo', 'flanging', 'flanking', 'flankers', 'flannels', 'flapjack', 'flapping', 'flappers', 'flashing', 'flashily', 'flashier', 'flatness', 'flatboat', 'flatcars', 'flatfeet', 'flatfish', 'flatfoot', 'flatiron', 'flatting', 'flatters', 'flattest', 'flattens', 'flattery', 'flattops', 'flatware', 'flatworm', 'flaunted', 'flautist', 'flavored', 'flawless', 'flaxseed', 'flecking', 'fledging', 'fleecing', 'fleecier', 'fleeting', 'fleetest', 'fleshing', 'fleshier', 'flexions', 'flexibly', 'flexible', 'flextime', 'flexures', 'flicking', 'flickers', 'flighted', 'flimflam', 'flimsily', 'flimsier', 'flimsies', 'flinched', 'flinches', 'flinging', 'flintier', 'flipping', 'flippers', 'flippest', 'flippant', 'flirting', 'flitting', 'floating', 'floaters', 'flocking', 'flogging', 'flooding', 'floodlit', 'flooring', 'floozies', 'flopping', 'floppier', 'floppies', 'florists', 'flossing', 'flossier', 'flotilla', 'flounces', 'flounced', 'flounder', 'flouring', 'flourish', 'flouting', 'flowered', 'fluently', 'fluffing', 'fluffier', 'fluidity', 'fluidics', 'flukiest', 'flummery', 'flunking', 'flunkies', 'fluoride', 'fluorine', 'fluorite', 'flurried', 'flurries', 'flushing', 'flusters', 'flutists', 'flutings', 'flutters', 'fluttery', 'flyaways', 'flyblown', 'flypaper', 'flyspeck', 'flytraps', 'flywheel', 'foamiest', 'focusing', 'foddered', 'foggiest', 'foghorns', 'foisting', 'folderol', 'foliages', 'foliated', 'foliates', 'folioing', 'folklore', 'folksier', 'folkways', 'follicle', 'followed', 'follower', 'fomented', 'fomenter', 'fondness', 'fondants', 'fondling', 'foolscap', 'footings', 'footages', 'football', 'footfall', 'footgear', 'foothill', 'foothold', 'footless', 'footling', 'footnote', 'footpath', 'footrest', 'footsore', 'footstep', 'footwear', 'footwork', 'foraging', 'foragers', 'foramina', 'foraying', 'forbears', 'forborne', 'forceful', 'forcibly', 'forcible', 'forearms', 'forebear', 'forebode', 'forecast', 'foredoom', 'forefeet', 'forefoot', 'foregoes', 'foregone', 'forehand', 'forehead', 'forelegs', 'forelimb', 'forelock', 'foremast', 'foremost', 'forename', 'forenoon', 'forensic', 'forepart', 'foresail', 'foresees', 'foreseen', 'foreskin', 'forested', 'forestry', 'foretell', 'foretold', 'forewing', 'forewarn', 'forewent', 'foreword', 'forfeits', 'forgings', 'forgiven', 'forgives', 'forgoing', 'forgoers', 'forkfuls', 'formally', 'formerly', 'formless', 'formulas', 'formulae', 'forsakes', 'forsaken', 'forsooth', 'forswear', 'forswore', 'forsworn', 'fortieth', 'fortiori', 'fortress', 'fortuity', 'fortunes', 'forwards', 'fostered', 'foulness', 'foulards', 'founding', 'founders', 'fountain', 'fourfold', 'foursome', 'fourteen', 'foxglove', 'foxholes', 'foxhound', 'foxiness', 'fracases', 'fraction', 'fracture', 'fragment', 'fragrant', 'frailest', 'framings', 'francium', 'franking', 'frankest', 'frazzles', 'frazzled', 'freaking', 'freakier', 'freakish', 'freckles', 'freckled', 'freebies', 'freeborn', 'freedoms', 'freeform', 'freehand', 'freehold', 'freeload', 'freesias', 'freeways', 'freewill', 'freezing', 'freezers', 'freights', 'frenetic', 'frenzies', 'frenzied', 'frequent', 'frescoes', 'freshest', 'freshens', 'freshets', 'freshmen', 'freshman', 'fretting', 'fretwork', 'freudian', 'friction', 'friendly', 'frigates', 'frighten', 'frijoles', 'frilling', 'frillier', 'fringing', 'frippery', 'frisbees', 'frisking', 'friskily', 'friskier', 'fritters', 'frizzing', 'frizzier', 'frizzles', 'frizzled', 'frocking', 'froggier', 'fronting', 'frontals', 'frontage', 'frontier', 'frosting', 'frostily', 'frostier', 'frosteds', 'frothing', 'frothily', 'frothier', 'frowning', 'frowzier', 'fructify', 'fructose', 'frugally', 'fruiting', 'fruitier', 'fruition', 'fruitful', 'frumpily', 'frumpier', 'frumpish', 'frustums', 'fuchsias', 'fuddling', 'fugitive', 'fulcrums', 'fulfills', 'fullback', 'fullness', 'fumbling', 'fumblers', 'fumigate', 'function', 'funerals', 'funerary', 'funereal', 'funkiest', 'funniest', 'funneled', 'furbelow', 'furlongs', 'furlough', 'furnaces', 'furriest', 'furrowed', 'furthers', 'furthest', 'fuselage', 'fusilier', 'fussiest', 'fustians', 'fustiest', 'futility', 'futurism', 'futurity', 'futurist', 'fuzziest', 'gabbiest', 'gabbling', 'gabfests', 'gadabout', 'gadflies', 'gadgetry', 'gaggling', 'gaieties', 'gainsaid', 'gainsays', 'galactic', 'galaxies', 'galilean', 'gallants', 'galleons', 'galloped', 'galoshes', 'galvanic', 'gambling', 'gamblers', 'gamboled', 'gamecock', 'gamester', 'gammoned', 'gangling', 'ganglion', 'ganglier', 'gangrene', 'gangster', 'gangways', 'gantlets', 'gantries', 'garaging', 'garbages', 'garbling', 'gardened', 'gardener', 'gardenia', 'gargling', 'gargoyle', 'garlands', 'garlicky', 'garments', 'garnered', 'garrison', 'garroted', 'garrotes', 'gartered', 'gaslight', 'gasoline', 'gassiest', 'gassings', 'gastrula', 'gasworks', 'gatefold', 'gatepost', 'gateways', 'gathered', 'gatherer', 'gatorade', 'gaudiest', 'gauntest', 'gauntlet', 'gaussian', 'gauziest', 'gaveling', 'gavottes', 'gawkiest', 'gayeties', 'gazelles', 'gazetted', 'gazettes', 'gearings', 'gelatins', 'geldings', 'geminate', 'gemology', 'gemsboks', 'gemstone', 'gendarme', 'generics', 'generals', 'generate', 'generous', 'genetics', 'genially', 'genitors', 'genitals', 'genitive', 'geniuses', 'genocide', 'genotype', 'gentians', 'gentiles', 'gentling', 'gentlest', 'gentries', 'geodesic', 'geodetic', 'geologic', 'geometer', 'geometry', 'geotaxes', 'geotaxis', 'geranium', 'germanic', 'germinal', 'gestalts', 'gestated', 'gestates', 'gestured', 'gestures', 'getaways', 'ghanaian', 'gherkins', 'ghosting', 'ghoulish', 'giantess', 'gibbered', 'gibbeted', 'giddying', 'giddiest', 'gigabits', 'gigantic', 'giggling', 'gildings', 'gimcrack', 'gimleted', 'gimmicks', 'gimmicky', 'gimpiest', 'gingered', 'gingerly', 'ginghams', 'gingival', 'ginkgoes', 'ginsengs', 'giraffes', 'girdling', 'girlhood', 'giveaway', 'gizzards', 'glaceing', 'glaciers', 'gladness', 'gladdest', 'gladdens', 'gladioli', 'glamours', 'glancing', 'glanders', 'glassing', 'glassily', 'glassier', 'glassful', 'glassine', 'glaucoma', 'glazings', 'glaziers', 'gleaming', 'gleaning', 'glibness', 'glibbest', 'glimmers', 'glimpsed', 'glimpses', 'glinting', 'glissade', 'glisters', 'glistens', 'glitches', 'glitters', 'glittery', 'gloaming', 'gloating', 'globally', 'globules', 'globular', 'globulin', 'glooming', 'gloomily', 'gloomier', 'glorying', 'glorious', 'glossing', 'glossily', 'glossier', 'glossary', 'glowered', 'glowworm', 'gloxinia', 'glucoses', 'glumness', 'glummest', 'glutting', 'gluttons', 'gluttony', 'glycerin', 'glycerol', 'glycogen', 'gnarling', 'gnashing', 'gneisses', 'gneissic', 'gnostics', 'goatskin', 'gobbling', 'gobblers', 'godchild', 'godheads', 'godhoods', 'godliest', 'godsends', 'godspeed', 'goggling', 'goldener', 'goldfish', 'goliaths', 'gondolas', 'gonfalon', 'goodlier', 'goodness', 'goodwill', 'goofiest', 'gorgeous', 'gorillas', 'goshawks', 'goslings', 'gossamer', 'gossiped', 'gouaches', 'gourmand', 'gourmets', 'goutiest', 'governed', 'governor', 'grabbing', 'grabbers', 'graceful', 'gracious', 'grackles', 'gradings', 'gradated', 'gradates', 'gradient', 'graduals', 'graduate', 'graffiti', 'graffito', 'grafting', 'graining', 'grainier', 'grammars', 'grandest', 'grandams', 'granddad', 'grandees', 'grandeur', 'grandmas', 'grandpas', 'grandson', 'grangers', 'granites', 'grannies', 'granting', 'granters', 'grantors', 'grantees', 'granules', 'granular', 'graphing', 'grapheme', 'graphics', 'graphite', 'grapnels', 'grapples', 'grappled', 'grasping', 'grassing', 'grassier', 'grateful', 'gratings', 'gratuity', 'graveled', 'graviton', 'gravures', 'grayness', 'grayling', 'grazings', 'graziers', 'greasing', 'greasers', 'greasily', 'greasier', 'greatest', 'greedily', 'greedier', 'greening', 'greenest', 'greenery', 'greenish', 'greeting', 'greeters', 'gremlins', 'grenaded', 'grenades', 'griddles', 'griddled', 'gridiron', 'grieving', 'grievous', 'griffins', 'grilling', 'grimiest', 'grimness', 'grimaced', 'grimaces', 'grimmest', 'grinding', 'grinders', 'grinning', 'gripping', 'grislier', 'gristles', 'gritting', 'grittier', 'grizzled', 'groaning', 'groggily', 'groggier', 'grograms', 'groining', 'grommets', 'grooming', 'grooving', 'groovily', 'groovier', 'grosbeak', 'grossing', 'grossest', 'grottoes', 'grouched', 'grouches', 'grounded', 'grouping', 'groupers', 'groupies', 'grousing', 'grouting', 'groveled', 'growling', 'grownups', 'grubbing', 'grubbily', 'grubbier', 'grudging', 'grueling', 'gruesome', 'gruffest', 'grumbles', 'grumbled', 'grumbler', 'grumping', 'grumpily', 'grumpier', 'grunions', 'grunting', 'guanacos', 'guanines', 'guaranty', 'guarding', 'guardian', 'gudgeons', 'guernsey', 'guessing', 'guesting', 'guffawed', 'guidance', 'guileful', 'guiltily', 'guiltier', 'gulfweed', 'gullying', 'gullibly', 'gullible', 'gumboils', 'gumdrops', 'gummiest', 'gumption', 'gumshoes', 'gumshoed', 'gumwoods', 'gunboats', 'gunfight', 'gunfires', 'gunlocks', 'gunmetal', 'gunshots', 'gunsmith', 'gunwales', 'gurgling', 'gushiest', 'gusseted', 'gustiest', 'gutsiest', 'guttered', 'guttural', 'guzzling', 'gymnasts', 'gyrating', 'gyration', 'gyrators', 'habiting', 'habitats', 'habitues', 'habitual', 'habitude', 'hacienda', 'hackling', 'hackneys', 'hacksaws', 'haggling', 'hagglers', 'hairiest', 'haircuts', 'hairless', 'hairline', 'hairpins', 'haitians', 'halation', 'halberds', 'haleness', 'halfback', 'halftone', 'hallmark', 'hallooed', 'hallowed', 'hallways', 'halogens', 'haltered', 'halyards', 'hamilton', 'hammered', 'hammocks', 'hampered', 'hamsters', 'handiest', 'handbags', 'handball', 'handbill', 'handbook', 'handcars', 'handcart', 'handcuff', 'handfuls', 'handguns', 'handicap', 'handling', 'handlers', 'handmade', 'handmaid', 'handouts', 'handrail', 'handsome', 'handwork', 'handymen', 'handyman', 'hangings', 'hangdogs', 'hangnail', 'hangouts', 'hangover', 'hankered', 'haploids', 'happiest', 'happened', 'harangue', 'harassed', 'harasses', 'harbored', 'hardiest', 'hardness', 'hardback', 'hardball', 'hardened', 'hardener', 'hardpans', 'hardship', 'hardtack', 'hardtops', 'hardware', 'hardwire', 'hardwood', 'harelips', 'haricots', 'harmless', 'harmonic', 'harpists', 'harpoons', 'harrying', 'harriers', 'harridan', 'harrowed', 'harshest', 'hartford', 'harvests', 'hashanah', 'hassling', 'hassocks', 'hastiest', 'hastened', 'hatboxes', 'hatching', 'hatchery', 'hatchets', 'hatchway', 'hauberks', 'haulages', 'haunches', 'haunting', 'hautboys', 'hauteurs', 'hawaiian', 'hawkweed', 'hawthorn', 'hayforks', 'haylofts', 'hayracks', 'hayseeds', 'haystack', 'haywires', 'hazarded', 'hazelnut', 'haziness', 'headiest', 'headings', 'headache', 'headachy', 'headband', 'headgear', 'headland', 'headless', 'headline', 'headlock', 'headlong', 'headrest', 'headroom', 'headsets', 'headways', 'headwork', 'healable', 'hearings', 'hearkens', 'hearsays', 'heartily', 'heartier', 'heartens', 'heatedly', 'heathens', 'heathers', 'heathery', 'heaviest', 'heavenly', 'heavyset', 'hecatomb', 'heckling', 'hecklers', 'hectares', 'hectored', 'hedgehog', 'hedgehop', 'hedgerow', 'hedonism', 'hedonist', 'heedless', 'heehawed', 'heftiest', 'hegemony', 'heighten', 'heirloom', 'heisting', 'helicons', 'heliport', 'hellions', 'hellcats', 'hellenic', 'hellfire', 'hellhole', 'helloing', 'helmeted', 'helmsmen', 'helmsman', 'helpings', 'helpless', 'helpmate', 'helpmeet', 'helsinki', 'hematite', 'hemlocks', 'hemostat', 'henbanes', 'henchmen', 'henchman', 'hennaing', 'henpecks', 'heparins', 'hepatica', 'heptagon', 'heralded', 'heraldic', 'heraldry', 'hercules', 'herdsmen', 'herdsman', 'heredity', 'hereford', 'heresies', 'heretics', 'hereunto', 'hereupon', 'herewith', 'heritage', 'hermetic', 'hermitic', 'heroisms', 'heroines', 'herpetic', 'hesitant', 'hesitate', 'hexagons', 'hexagram', 'hexapods', 'hiatuses', 'hibachis', 'hibiscus', 'hidalgos', 'hideaway', 'highball', 'highborn', 'highboys', 'highbred', 'highbrow', 'highland', 'highness', 'highroad', 'hightail', 'highways', 'hijacked', 'hijacker', 'hilarity', 'hilliest', 'hillocks', 'hillside', 'hilltops', 'hindered', 'hindmost', 'hinduism', 'hipbones', 'hipsters', 'hireling', 'hispanic', 'historic', 'hitching', 'hitherto', 'hoariest', 'hoarding', 'hoarsest', 'hoarsely', 'hobbling', 'hobblers', 'hobbyist', 'hobnails', 'hoecakes', 'hogbacks', 'hogshead', 'hoisting', 'holdings', 'holdouts', 'holdover', 'holidays', 'holiness', 'holistic', 'hollered', 'hollowed', 'holocene', 'hologram', 'holsters', 'holstein', 'homburgs', 'homebody', 'homebred', 'homelier', 'homeland', 'homeless', 'homelike', 'homemade', 'homeroom', 'homesick', 'homespun', 'homeward', 'homework', 'homicide', 'homilies', 'hominies', 'hominids', 'hominoid', 'homology', 'homonyms', 'honchoed', 'honduras', 'honduran', 'honeying', 'honestly', 'honeybee', 'honeydew', 'honolulu', 'honoring', 'honorary', 'hoodlums', 'hoodwink', 'hookworm', 'hooligan', 'hoosegow', 'hoosiers', 'hopefuls', 'hopeless', 'horizons', 'hormones', 'hormonal', 'horniest', 'hornball', 'hornbook', 'hornpipe', 'horologe', 'horology', 'horribly', 'horrible', 'horrific', 'horsiest', 'horsecar', 'horsefly', 'horsemen', 'horseman', 'hosannas', 'hospices', 'hospital', 'hostages', 'hostlers', 'hotfoots', 'hotheads', 'hothouse', 'hotshots', 'hounding', 'housings', 'housefly', 'housetop', 'hovering', 'howitzer', 'huckster', 'huddling', 'huffiest', 'hugeness', 'huguenot', 'humanism', 'humanity', 'humanely', 'humanist', 'humanize', 'humanoid', 'humbling', 'humblest', 'humdrums', 'humerals', 'humidors', 'humidity', 'humidify', 'humility', 'hummocks', 'humoring', 'humorist', 'humorous', 'humpback', 'hunching', 'hundreds', 'hungered', 'hungrily', 'hungrier', 'hunkered', 'huntress', 'huntsmen', 'huntsman', 'hurdling', 'hurrying', 'hurrahed', 'hurtling', 'husbands', 'huskiest', 'hustling', 'hustlers', 'hyalines', 'hyalites', 'hydrants', 'hydrated', 'hydrates', 'hydrator', 'hydrides', 'hydrogen', 'hydroids', 'hydroxyl', 'hygienes', 'hygienic', 'hymeneal', 'hypnoses', 'hypnosis', 'hypnotic', 'hysteria', 'hysteric', 'iberians', 'icebergs', 'iceboats', 'icebound', 'iceboxes', 'idealist', 'idealism', 'idealize', 'ideating', 'ideation', 'identity', 'identify', 'ideogram', 'ideology', 'idiocies', 'idleness', 'idolater', 'idolatry', 'idolized', 'idolizes', 'igniting', 'ignition', 'ignominy', 'ignoring', 'ignorant', 'illinois', 'illiquid', 'illumine', 'illusion', 'illusive', 'illusory', 'imagined', 'imagines', 'imbecile', 'imbibing', 'imbruing', 'imitated', 'imitates', 'imitator', 'immanent', 'immature', 'immersed', 'immerses', 'imminent', 'immobile', 'immodest', 'immolate', 'immortal', 'immunity', 'immunize', 'immuring', 'impacted', 'impaired', 'impaling', 'impanels', 'imparted', 'impasses', 'impeding', 'impelled', 'impeller', 'impended', 'imperial', 'imperils', 'impetigo', 'impinged', 'impinger', 'impinges', 'impishly', 'implying', 'implants', 'implicit', 'imploded', 'implodes', 'implored', 'implores', 'impolite', 'imported', 'importer', 'imposing', 'impostor', 'impotent', 'impounds', 'imprints', 'imprison', 'improper', 'improved', 'improves', 'impudent', 'impugned', 'impulses', 'impunity', 'impurity', 'imputing', 'inaction', 'inactive', 'inarches', 'inasmuch', 'inboards', 'inbreeds', 'incensed', 'incenses', 'inchoate', 'inchworm', 'incident', 'incising', 'incision', 'incisive', 'incisors', 'inciting', 'inclined', 'inclines', 'included', 'includes', 'incoming', 'increase', 'incubate', 'incurred', 'indebted', 'indecent', 'indented', 'indenter', 'indexing', 'indicate', 'indicted', 'indicter', 'indictor', 'indigent', 'indirect', 'inditing', 'indolent', 'inducing', 'inducted', 'inductor', 'inductee', 'indulged', 'indulges', 'industry', 'inedible', 'inequity', 'inertial', 'inertias', 'inexpert', 'infamies', 'infamous', 'infantry', 'infected', 'inferior', 'infernal', 'infernos', 'inferred', 'infested', 'infidels', 'infields', 'infinity', 'infinite', 'inflamed', 'inflames', 'inflated', 'inflates', 'inflects', 'inflicts', 'influxes', 'informed', 'informer', 'informal', 'infrared', 'infringe', 'infusing', 'infusion', 'ingenues', 'ingested', 'ingrains', 'ingrates', 'inguinal', 'inhabits', 'inhaling', 'inhalers', 'inhalant', 'inhering', 'inherent', 'inherits', 'inhibits', 'inhuming', 'inhumane', 'inimical', 'iniquity', 'initials', 'initiate', 'injected', 'injector', 'injuring', 'injuries', 'inkblots', 'inkhorns', 'inkiness', 'inklings', 'inkstand', 'inkwells', 'inlaying', 'innocent', 'innovate', 'innuendo', 'inputted', 'inquests', 'inquired', 'inquirer', 'inquires', 'inrushes', 'insanity', 'insanely', 'inscribe', 'insecure', 'inserted', 'insiders', 'insights', 'insignia', 'insisted', 'insolent', 'insomnia', 'insomuch', 'inspects', 'inspired', 'inspirer', 'inspires', 'inspirit', 'instance', 'installs', 'instants', 'instated', 'instates', 'instills', 'instinct', 'instruct', 'insulate', 'insulins', 'insulted', 'insuring', 'insurers', 'intaglio', 'integers', 'integral', 'intended', 'intently', 'interact', 'intercom', 'interest', 'interior', 'interims', 'intermit', 'intermix', 'interned', 'internal', 'internee', 'interred', 'interval', 'intimacy', 'intimate', 'intoning', 'intrepid', 'intrigue', 'introits', 'intruded', 'intruder', 'intrudes', 'intuited', 'inundate', 'invading', 'invaders', 'invalids', 'invasion', 'invasive', 'inveigle', 'inveighs', 'invented', 'inventor', 'inverses', 'inverted', 'invested', 'investor', 'inviting', 'invoiced', 'invoices', 'invoking', 'involved', 'involves', 'inwardly', 'iodizing', 'ionizing', 'iranians', 'ironclad', 'ironical', 'ironware', 'ironwood', 'ironwork', 'iroquois', 'irrigate', 'irritant', 'irritate', 'irrupted', 'islander', 'isobaric', 'isogonic', 'isolated', 'isolates', 'isolator', 'isomeric', 'isoprene', 'isotherm', 'isotopes', 'isotopic', 'isotropy', 'israelis', 'issuance', 'isthmian', 'italians', 'itchiest', 'itemized', 'itemizes', 'iterated', 'iterates', 'jabbered', 'jackboot', 'jackdaws', 'jacketed', 'jackpots', 'jacquard', 'jadeites', 'jaggedly', 'jailbird', 'jalopies', 'jalousie', 'jamaican', 'jamboree', 'jangling', 'janitors', 'japanese', 'japanned', 'japeries', 'japonica', 'jaundice', 'jaunting', 'jauntily', 'jauntier', 'javanese', 'javelins', 'jawboned', 'jawbones', 'jaywalks', 'jazziest', 'jealousy', 'jellying', 'jeopardy', 'jeremiad', 'jerkiest', 'jetliner', 'jetports', 'jettison', 'jeweling', 'jewelers', 'jezebels', 'jiggered', 'jiggling', 'jigsawed', 'jimmying', 'jingling', 'jingoism', 'jingoish', 'jingoist', 'jockeyed', 'jocosity', 'jodhpurs', 'joggling', 'jointing', 'jointers', 'jointure', 'jokester', 'jokingly', 'jolliest', 'jonquils', 'jostling', 'jottings', 'jouncing', 'journals', 'journeys', 'jousting', 'joyfully', 'joyously', 'jubilant', 'jubilees', 'judgment', 'judicial', 'juggling', 'jugglers', 'jugulars', 'juiciest', 'julienne', 'jumbling', 'jumpiest', 'junction', 'juncture', 'junipers', 'junketed', 'justness', 'justices', 'juvenile', 'kamikaze', 'kangaroo', 'karakuls', 'katydids', 'kayaking', 'kedgeree', 'keelboat', 'keelhaul', 'keenness', 'keepsake', 'kenneled', 'kentucky', 'keratins', 'kerchief', 'kerosene', 'kestrels', 'keyboard', 'keyholes', 'keynoted', 'keynoter', 'keynotes', 'keypunch', 'keystone', 'keywords', 'khartoum', 'khedives', 'kibitzed', 'kibitzer', 'kibitzes', 'kiboshed', 'kiboshes', 'kickback', 'kickball', 'kickoffs', 'kidnaped', 'kidnaper', 'kidskins', 'killings', 'killdeer', 'killjoys', 'kilobits', 'kilobyte', 'kilogram', 'kilotons', 'kilowatt', 'kindling', 'kindlier', 'kindness', 'kinetics', 'kingbird', 'kingbolt', 'kingdoms', 'kinglier', 'kingpins', 'kingship', 'kinkiest', 'kinkajou', 'kinsfolk', 'kinships', 'kippered', 'kirsches', 'kitchens', 'kitsches', 'klansmen', 'klansman', 'kludging', 'knapsack', 'kneading', 'kneecaps', 'kneehole', 'kneeling', 'kneepads', 'knelling', 'knickers', 'knighted', 'knightly', 'knitting', 'knitters', 'knitwear', 'knobbier', 'knocking', 'knockers', 'knockoff', 'knockout', 'knothole', 'knotting', 'knottier', 'knowable', 'knuckles', 'knuckled', 'knurling', 'kohlrabi', 'kookiest', 'kowtowed', 'kumquats', 'labeling', 'labelers', 'laboring', 'laborers', 'laborite', 'labrador', 'laburnum', 'lacerate', 'lacewing', 'laciness', 'lacquers', 'lacrosse', 'lactases', 'lactated', 'lactates', 'lacteals', 'lactoses', 'ladybird', 'ladybugs', 'ladylike', 'ladylove', 'ladyship', 'laetrile', 'laggards', 'lakeside', 'lamasery', 'lambaste', 'lambency', 'lambskin', 'lamellae', 'lamellar', 'lamented', 'laminate', 'lampoons', 'lamppost', 'lampreys', 'lancelet', 'lancelot', 'landings', 'landfall', 'landlady', 'landlord', 'landmark', 'landward', 'language', 'languish', 'languors', 'lankiest', 'lanolins', 'lanterns', 'lanyards', 'laotians', 'lapidary', 'lapwings', 'larboard', 'larcener', 'larkspur', 'larruped', 'larynges', 'larynxes', 'lashings', 'lassoing', 'latching', 'latchkey', 'lateness', 'laterals', 'lathered', 'latitude', 'latrines', 'latterly', 'latticed', 'lattices', 'latvians', 'laudably', 'laudable', 'laudanum', 'laughing', 'laughter', 'launched', 'launcher', 'launches', 'launders', 'laureate', 'lavatory', 'lavender', 'lavished', 'lavishes', 'lavishly', 'lawfully', 'lawgiver', 'lawmaker', 'lawsuits', 'laxative', 'laxities', 'layaways', 'layering', 'layettes', 'layovers', 'laziness', 'leaching', 'leadings', 'leadoffs', 'leafiest', 'leaflets', 'leaguing', 'leaguers', 'leakiest', 'leakages', 'leanings', 'leapfrog', 'learning', 'learners', 'leasable', 'leashing', 'leathers', 'leathery', 'leavings', 'leavened', 'lebanese', 'lecithin', 'lecterns', 'lectured', 'lecturer', 'lectures', 'leeching', 'leeriest', 'leewards', 'leftists', 'leftmost', 'leftover', 'legacies', 'legalism', 'legality', 'legalist', 'legalize', 'legation', 'legatees', 'leggiest', 'leggings', 'leghorns', 'leisures', 'leisured', 'lemmings', 'lemonier', 'lemonade', 'lengthen', 'leniency', 'lenience', 'leninist', 'lenities', 'lenitive', 'leopards', 'leotards', 'lesbians', 'lessened', 'letdowns', 'lethargy', 'lettered', 'leukemia', 'leveling', 'levelers', 'levering', 'leverage', 'levities', 'levitate', 'lewdness', 'lexicons', 'liaisons', 'libation', 'libeling', 'libelers', 'libelous', 'liberals', 'liberate', 'liberian', 'libretto', 'licensed', 'licenser', 'licenses', 'licensee', 'lickings', 'licorice', 'lifeboat', 'lifeless', 'lifelike', 'lifeline', 'lifelong', 'lifetime', 'lifework', 'liftoffs', 'ligament', 'ligature', 'lighting', 'lighters', 'lightest', 'lightens', 'ligneous', 'lignites', 'ligroins', 'likelier', 'likening', 'likeness', 'likewise', 'limbered', 'limeades', 'limerick', 'limiting', 'limonite', 'linchpin', 'lineages', 'linearly', 'linefeed', 'linesmen', 'linesman', 'lingered', 'lingerer', 'lingerie', 'linguini', 'linguist', 'liniment', 'linkings', 'linkages', 'linnaean', 'linoleum', 'linotype', 'linseeds', 'lionized', 'lionizes', 'lipstick', 'liqueurs', 'listings', 'listened', 'listener', 'listless', 'litanies', 'literacy', 'literary', 'literate', 'literati', 'lithiums', 'litigant', 'litigate', 'litmuses', 'littered', 'littlest', 'littoral', 'livelier', 'livelong', 'livening', 'liveries', 'liveried', 'loamiest', 'loathing', 'lobbying', 'lobbyist', 'lobelias', 'loblolly', 'lobotomy', 'lobsters', 'locality', 'localize', 'locating', 'location', 'locators', 'locative', 'lockjaws', 'lockouts', 'locoweed', 'locution', 'lodestar', 'lodgings', 'lodgment', 'loftiest', 'logbooks', 'logicals', 'logician', 'logistic', 'logotype', 'loitered', 'loiterer', 'lollipop', 'londoner', 'lonelier', 'lonesome', 'longings', 'longboat', 'longbows', 'longhair', 'longhand', 'longhorn', 'longleaf', 'longlegs', 'lookouts', 'looniest', 'loophole', 'loosened', 'lopsided', 'lordlier', 'lordship', 'lothario', 'loudness', 'lounging', 'lousiest', 'louvered', 'lovebird', 'lovelier', 'lovelies', 'loveless', 'lovelorn', 'lovesick', 'lovingly', 'lowbrows', 'lowdowns', 'lowering', 'lowliest', 'lowlands', 'loyalist', 'lozenged', 'lozenges', 'lucidity', 'luckiest', 'luckless', 'lukewarm', 'lumbered', 'lumbagos', 'luminary', 'luminous', 'lummoxes', 'lumpiest', 'lunacies', 'lunatics', 'lunching', 'luncheon', 'lunettes', 'lungfish', 'lurching', 'luscious', 'lustiest', 'lustrous', 'lutetium', 'lutheran', 'luxuries', 'lymphoid', 'lynching', 'lyrebird', 'lyricist', 'lyricism', 'lysergic', 'macaques', 'macaroni', 'macaroon', 'macerate', 'machetes', 'machined', 'machines', 'machismo', 'mackerel', 'mackinaw', 'maddened', 'madhouse', 'madonnas', 'madrases', 'madrigal', 'madwomen', 'madwoman', 'maestros', 'magazine', 'magcards', 'magentas', 'magician', 'magnates', 'magnesia', 'magnetic', 'magnetos', 'magnolia', 'maharani', 'mahatmas', 'mahjongs', 'mahogany', 'maidenly', 'mailings', 'mailbags', 'mailgram', 'mainland', 'mainline', 'mainmast', 'mainsail', 'mainstay', 'maintain', 'maintops', 'majestic', 'majolica', 'majoring', 'majority', 'maladies', 'malagasy', 'malaises', 'malamute', 'malarial', 'malarias', 'malarkey', 'malaysia', 'maldives', 'maleness', 'maligned', 'malinger', 'mallards', 'malmseys', 'maltases', 'maltoses', 'maltreat', 'mammoths', 'manacles', 'manacled', 'managing', 'managers', 'manatees', 'mandamus', 'mandarin', 'mandated', 'mandates', 'mandible', 'mandolin', 'mandrake', 'mandrels', 'mandrill', 'maneuver', 'manfully', 'mangiest', 'mangling', 'mangrove', 'manholes', 'manhoods', 'manhunts', 'maniacal', 'manicure', 'manifest', 'manifold', 'manikins', 'maniples', 'manliest', 'mannered', 'mannerly', 'manorial', 'manpower', 'mansions', 'mansards', 'mantilla', 'mantises', 'mantissa', 'mantling', 'manually', 'manumits', 'marabous', 'marathon', 'marauded', 'marauder', 'marbling', 'marching', 'marchers', 'margined', 'marginal', 'marigold', 'marimbas', 'mariners', 'marinade', 'marinate', 'mariposa', 'maritime', 'marjoram', 'markings', 'markdown', 'markedly', 'marketed', 'marketer', 'marksmen', 'marksman', 'marlines', 'marmoset', 'marooned', 'marquees', 'marquise', 'marrying', 'marriage', 'marshier', 'marshals', 'martians', 'martinet', 'martinis', 'martyred', 'marveled', 'marxists', 'maryland', 'marzipan', 'massacre', 'massaged', 'massages', 'masseurs', 'masseuse', 'mastered', 'masterly', 'masthead', 'mastiffs', 'mastitis', 'mastodon', 'mastoids', 'matadors', 'matching', 'matchbox', 'material', 'materiel', 'maternal', 'matinees', 'matrices', 'matrixes', 'matronly', 'mattings', 'mattered', 'mattocks', 'mattress', 'maturing', 'maturity', 'maturate', 'maunders', 'maverick', 'maxillae', 'maxillar', 'maximize', 'maximums', 'mayflies', 'mazurkas', 'mealtime', 'meanness', 'meanings', 'meanders', 'meantime', 'measlier', 'measured', 'measurer', 'measures', 'meatiest', 'meatball', 'mechanic', 'meddling', 'meddlers', 'mediated', 'mediates', 'mediator', 'medicaid', 'medicare', 'medicate', 'medicine', 'medieval', 'mediocre', 'meditate', 'medullas', 'medullar', 'meetings', 'megabits', 'megabyte', 'megalith', 'megatons', 'megawatt', 'melamine', 'melanges', 'melanins', 'melanoma', 'meldings', 'mellitus', 'mellowed', 'mellower', 'melodies', 'melodeon', 'meltable', 'meltdown', 'membrane', 'mementos', 'memories', 'memorial', 'memoriam', 'memorize', 'menacing', 'menhaden', 'meninges', 'meniscal', 'meniscus', 'mentally', 'menthols', 'mentions', 'mephitic', 'mephitis', 'merchant', 'merciful', 'mercuric', 'meridian', 'meringue', 'meriting', 'mermaids', 'merriest', 'meshwork', 'mesoderm', 'mesozoic', 'mesquite', 'messiest', 'messages', 'metallic', 'metaphor', 'metazoan', 'meteoric', 'metering', 'methanes', 'methanol', 'methinks', 'metonyms', 'metonymy', 'metrical', 'mexicans', 'mezuzahs', 'michigan', 'microbes', 'middling', 'midlands', 'midnight', 'midpoint', 'midriffs', 'midterms', 'midweeks', 'midwives', 'midyears', 'mightily', 'mightier', 'migraine', 'migrants', 'migrated', 'migrates', 'milanese', 'mildness', 'mildewed', 'mileages', 'milepost', 'militant', 'military', 'militate', 'militias', 'milkiest', 'milkmaid', 'milksops', 'milkweed', 'millions', 'milldams', 'milliard', 'milliner', 'millrace', 'mimicked', 'mimicker', 'minarets', 'minatory', 'mindless', 'minerals', 'mingling', 'minicabs', 'minidisk', 'minimize', 'minimums', 'minister', 'ministry', 'minority', 'minotaur', 'minstrel', 'mintages', 'minuends', 'minutely', 'minutiae', 'miracles', 'mirrored', 'mirthful', 'misapply', 'miscalls', 'miscarry', 'miscasts', 'mischief', 'miscible', 'miscount', 'miscuing', 'misdeals', 'misdealt', 'misdeeds', 'misdoing', 'miseries', 'misfired', 'misfires', 'misguide', 'mishears', 'misheard', 'mishmash', 'misjudge', 'misleads', 'mismatch', 'misnamed', 'misnames', 'misnomer', 'misogamy', 'misogyny', 'misplace', 'misplays', 'misprint', 'misquote', 'misreads', 'misrules', 'misruled', 'missions', 'misshape', 'missiles', 'missives', 'missouri', 'misspell', 'misspend', 'misspent', 'misstate', 'missteps', 'mistiest', 'mistakes', 'mistaken', 'mistrals', 'mistreat', 'mistress', 'mistrial', 'mistrust', 'misusing', 'mitering', 'mitigate', 'mitzvahs', 'mixtures', 'mnemonic', 'mobility', 'mobilize', 'mobsters', 'moccasin', 'modality', 'modeling', 'moderate', 'moderato', 'modestly', 'modicums', 'modified', 'modifier', 'modifies', 'modulate', 'mohammed', 'moieties', 'moistest', 'moistens', 'moisture', 'molasses', 'moldiest', 'moldable', 'moldings', 'moldered', 'molecule', 'molehill', 'moleskin', 'molested', 'mollusks', 'momentum', 'monarchs', 'monarchy', 'monastic', 'monaural', 'monazite', 'monetary', 'moneybag', 'mongered', 'mongolia', 'mongoose', 'mongrels', 'monikers', 'monistic', 'monition', 'monitors', 'monitory', 'monkeyed', 'monocles', 'monodies', 'monodist', 'monogamy', 'monogram', 'monolith', 'monomers', 'monomial', 'monopoly', 'monorail', 'monotone', 'monotony', 'monotype', 'monoxide', 'monsoons', 'monsters', 'montages', 'monument', 'mooching', 'moochers', 'moodiest', 'mooniest', 'moonbeam', 'mooncalf', 'moorings', 'moorages', 'moraines', 'morality', 'moralist', 'moralize', 'morasses', 'mordancy', 'mordents', 'moreover', 'moribund', 'mornings', 'moroccan', 'morosely', 'morpheme', 'morpheus', 'morphine', 'mortally', 'mortared', 'mortgage', 'mortised', 'mortises', 'mortuary', 'moseying', 'mosquito', 'mossiest', 'mossback', 'mothered', 'mothball', 'motherly', 'motility', 'motioned', 'motivate', 'motoring', 'motorist', 'motorcar', 'motorize', 'motormen', 'motorman', 'mottling', 'mounding', 'mounting', 'mountain', 'mourning', 'mourners', 'mournful', 'mousiest', 'mouthing', 'mouthful', 'movement', 'movingly', 'mucilage', 'muckrake', 'muddying', 'muddiest', 'muddling', 'mudguard', 'muezzins', 'muffling', 'mufflers', 'muggiest', 'muggings', 'mulattos', 'mulberry', 'mulching', 'mulcting', 'muleteer', 'mullions', 'mulleins', 'mulligan', 'multiple', 'multiply', 'mumbling', 'munching', 'muralist', 'murdered', 'murderer', 'murkiest', 'murmured', 'murrains', 'murcatel', 'muscling', 'muscular', 'mushiest', 'mushroom', 'musicals', 'musicale', 'musician', 'musketry', 'muskrats', 'mustiest', 'mustered', 'mustache', 'mustangs', 'mustards', 'mutating', 'mutation', 'mutative', 'muteness', 'mutilate', 'mutinous', 'mutineer', 'mutinied', 'mutinies', 'muttered', 'mutually', 'muzzling', 'mycelium', 'mycology', 'myrmidon', 'mystical', 'mystique', 'mythical', 'nacelles', 'nacreous', 'naivetes', 'nameless', 'namesake', 'namibian', 'nankeens', 'naperies', 'napoleon', 'narcoses', 'narcosis', 'narcotic', 'narrated', 'narrates', 'narrator', 'narrowed', 'narrower', 'narrowly', 'narwhals', 'nasality', 'nascence', 'nastiest', 'national', 'nativity', 'nattiest', 'naturals', 'nauseous', 'nauseate', 'nautical', 'nautilus', 'navigate', 'nazarene', 'nearness', 'neatness', 'nebraska', 'nebulous', 'necklace', 'neckline', 'neckties', 'neckwear', 'neediest', 'needling', 'needless', 'negating', 'negation', 'negative', 'neglects', 'negligee', 'neighbor', 'nektonic', 'nematode', 'nembutal', 'neomycin', 'neonates', 'neonatal', 'neophyte', 'neoplasm', 'neoprene', 'nepalese', 'nepenthe', 'nephrite', 'nepotism', 'nerviest', 'nervosas', 'nestling', 'netsukes', 'nettling', 'networks', 'neuritis', 'neuroses', 'neurosis', 'neurotic', 'neutered', 'neutrals', 'neutrino', 'neutrons', 'newborns', 'newcomer', 'newlywed', 'newsiest', 'newsboys', 'newscast', 'newsreel', 'nibbling', 'niceties', 'nickname', 'nicotine', 'niftiest', 'nigerian', 'niggling', 'nightcap', 'nihilism', 'nihilist', 'nimblest', 'ninepins', 'nineties', 'nineteen', 'nippiest', 'nirvanas', 'nitrated', 'nitrates', 'nitrides', 'nitrites', 'nitrogen', 'nobelium', 'nobility', 'noblemen', 'nobleman', 'noblesse', 'nobodies', 'nocturne', 'noisiest', 'nominate', 'nominees', 'nomogram', 'nonagons', 'nonesuch', 'nonjuror', 'nonmetal', 'nonsense', 'nonunion', 'nonwhite', 'noodling', 'noondays', 'noontide', 'noontime', 'normally', 'normalcy', 'northern', 'nosegays', 'nosiness', 'nostrils', 'nostrums', 'notables', 'notaries', 'notarize', 'notation', 'notching', 'notebook', 'nothings', 'noticing', 'notified', 'notifies', 'notional', 'nouveaux', 'novelist', 'novellas', 'november', 'novocain', 'nowadays', 'nuclease', 'nucleate', 'nucleoli', 'nucleons', 'nudities', 'nugatory', 'nuisance', 'numbness', 'numbered', 'numerals', 'numerous', 'numerate', 'numinous', 'numskull', 'nuptials', 'nursling', 'nurtured', 'nurtures', 'nuthatch', 'nutmeats', 'nutrient', 'nutshell', 'nuttiest', 'nuzzling', 'nymphets', 'oarlocks', 'oatmeals', 'obduracy', 'obdurate', 'obedient', 'obeisant', 'obelisks', 'obituary', 'objected', 'objector', 'oblation', 'obligate', 'obliging', 'obliques', 'oblivion', 'obscener', 'obscured', 'obscurer', 'obscures', 'observed', 'observer', 'observes', 'obsessed', 'obsesses', 'obsidian', 'obsolete', 'obstacle', 'obstruct', 'obtained', 'obtruded', 'obtrudes', 'obverses', 'obverted', 'obviated', 'obviates', 'obviator', 'ocarinas', 'occasion', 'occident', 'occluded', 'occludes', 'occupant', 'occupied', 'occupier', 'occupies', 'occurred', 'ocotillo', 'octagons', 'oculists', 'oddballs', 'oddities', 'oddments', 'odometer', 'odorless', 'odysseus', 'odysseys', 'offbeats', 'offended', 'offender', 'offenses', 'offering', 'officers', 'official', 'offshoot', 'offshore', 'ogresses', 'ohmmeter', 'oilcloth', 'oiliness', 'oilskins', 'oilstone', 'ointment', 'oklahoma', 'oldsters', 'oleander', 'oligarch', 'olivines', 'olympics', 'olympian', 'olympiad', 'omicrons', 'omission', 'omitting', 'omnivore', 'oncology', 'oncoming', 'onlooker', 'onrushes', 'ontology', 'ooziness', 'opaquing', 'openness', 'openings', 'openwork', 'operably', 'operable', 'operands', 'operated', 'operates', 'operatic', 'operator', 'operetta', 'ophidian', 'opiating', 'opinions', 'opponent', 'opposing', 'opposite', 'optician', 'optimism', 'optimist', 'optimize', 'optioned', 'optional', 'opulence', 'oracular', 'orations', 'oratorio', 'orbiting', 'orchards', 'ordained', 'ordering', 'ordinals', 'ordinary', 'ordinate', 'ordnance', 'organist', 'organism', 'organize', 'orgasmic', 'oriented', 'oriental', 'orifices', 'original', 'ornament', 'ornately', 'ornerier', 'orphaned', 'orpiment', 'orthicon', 'orthodox', 'osculate', 'ossified', 'ossifies', 'ottomans', 'outbacks', 'outboard', 'outbound', 'outbreak', 'outburst', 'outcasts', 'outclass', 'outcomes', 'outcries', 'outcrops', 'outdated', 'outdates', 'outdoing', 'outdoors', 'outfaced', 'outfaces', 'outfield', 'outflank', 'outflows', 'outfoxed', 'outfoxes', 'outgoing', 'outgrows', 'outgrown', 'outguess', 'outhouse', 'outlying', 'outlasts', 'outlawed', 'outlawry', 'outlined', 'outlines', 'outlived', 'outlives', 'outlooks', 'outmoded', 'outplays', 'outposts', 'outraged', 'outrages', 'outranks', 'outreach', 'outrider', 'outright', 'outsells', 'outshine', 'outshone', 'outsider', 'outsides', 'outsized', 'outsizes', 'outsmart', 'outstays', 'outstrip', 'outthink', 'outwards', 'outwears', 'outweigh', 'outworks', 'ovations', 'ovenbird', 'overacts', 'overages', 'overalls', 'overawed', 'overawes', 'overbids', 'overbite', 'overbook', 'overcall', 'overcame', 'overcast', 'overcoat', 'overcome', 'overdoes', 'overdone', 'overdose', 'overdraw', 'overdrew', 'overeats', 'overflow', 'overgrew', 'overgrow', 'overhand', 'overhang', 'overhaul', 'overhead', 'overhear', 'overheat', 'overhung', 'overkill', 'overlaid', 'overlain', 'overland', 'overlaps', 'overlays', 'overleap', 'overlies', 'overload', 'overlong', 'overlook', 'overlord', 'overmuch', 'overpaid', 'overpass', 'overpays', 'overplay', 'overrate', 'override', 'overripe', 'overrode', 'overrule', 'overruns', 'overseer', 'overseen', 'overseas', 'oversees', 'oversell', 'overshoe', 'overshot', 'oversize', 'oversold', 'overstep', 'overtake', 'overtime', 'overtone', 'overtook', 'overture', 'overturn', 'overused', 'overuses', 'overview', 'overwork', 'oviducts', 'ovulated', 'ovulates', 'owlishly', 'oxbloods', 'oxidants', 'oxidized', 'oxidizer', 'oxidizes', 'oxonians', 'oxygenic', 'oxymoron', 'pabulums', 'pacifist', 'pacifism', 'pacified', 'pacifies', 'pacifier', 'packings', 'packaged', 'packages', 'packsack', 'paddling', 'paddocks', 'padlocks', 'paganism', 'pageants', 'pageboys', 'paginate', 'painless', 'painting', 'painters', 'pairings', 'paisleys', 'pakistan', 'paladins', 'palatial', 'palatine', 'palavers', 'paleness', 'paleface', 'palettes', 'palfreys', 'palimony', 'palinode', 'palisade', 'palliate', 'palmiest', 'palmists', 'palmetto', 'palomino', 'palpably', 'palpable', 'palpated', 'palpates', 'paltered', 'paltrier', 'pampered', 'pamphlet', 'panaceas', 'panaches', 'panatela', 'pancaked', 'pancakes', 'pancreas', 'pandered', 'pandemic', 'pandowdy', 'paneling', 'panelist', 'pangolin', 'panicles', 'panicked', 'panniers', 'panorama', 'panpipes', 'panthers', 'pantheon', 'pantries', 'pantsuit', 'papacies', 'papering', 'papillae', 'papooses', 'parables', 'parabola', 'parading', 'paradigm', 'paradise', 'paraffin', 'paragons', 'paraguay', 'parakeet', 'parallax', 'parallel', 'paralyze', 'paramour', 'paranoia', 'paranoid', 'parapets', 'parasite', 'parasols', 'parboils', 'parceled', 'parching', 'pardoned', 'parented', 'parental', 'paretics', 'parfaits', 'parietal', 'parishes', 'parisian', 'parities', 'parkways', 'parlance', 'parlayed', 'parleyed', 'parmesan', 'parodied', 'parodies', 'paroling', 'parolees', 'parotids', 'paroxysm', 'parquets', 'parrying', 'parroted', 'parsnips', 'partings', 'partakes', 'partaken', 'parterre', 'particle', 'partisan', 'partners', 'parvenus', 'passions', 'passably', 'passable', 'passings', 'passages', 'passbook', 'passkeys', 'passover', 'passport', 'password', 'pastiest', 'pasterns', 'pastiche', 'pastille', 'pastimes', 'pastoral', 'pastries', 'pastrami', 'pastured', 'pastures', 'patching', 'patellae', 'patellar', 'patented', 'patently', 'patentee', 'paternal', 'pathetic', 'pathogen', 'pathways', 'patience', 'patients', 'patriots', 'patroons', 'pattered', 'patterns', 'paunches', 'paupered', 'pavement', 'pavilion', 'pawnshop', 'paycheck', 'payloads', 'payments', 'payrolls', 'peaceful', 'peachier', 'peacocks', 'pealings', 'pearling', 'pearlier', 'peasants', 'pebbling', 'pectoral', 'pectoris', 'peculate', 'peculiar', 'pedagogy', 'pedaling', 'pedantic', 'pedantry', 'peddling', 'peddlers', 'pedestal', 'pediment', 'pedicure', 'pedigree', 'peelings', 'peephole', 'peepshow', 'peerages', 'peerless', 'pegboard', 'peignoir', 'pelicans', 'pelisses', 'pellagra', 'pelleted', 'pellucid', 'pelvises', 'pemmican', 'penances', 'penalize', 'penchant', 'penciled', 'pendants', 'pendular', 'pendulum', 'penguins', 'penitent', 'penknife', 'pennants', 'penology', 'pensions', 'pentacle', 'pentagon', 'pentanes', 'penuches', 'penumbra', 'penuries', 'peonages', 'peopling', 'peppiest', 'peppered', 'peptides', 'peptones', 'percales', 'perceive', 'perching', 'perfects', 'perfecto', 'perforce', 'performs', 'perfumed', 'perfumes', 'pergolas', 'perigees', 'periling', 'perilous', 'periodic', 'perished', 'perishes', 'periwigs', 'perjured', 'perjurer', 'perjures', 'perkiest', 'permeate', 'permuted', 'permutes', 'peroxide', 'persians', 'persists', 'personas', 'personae', 'personal', 'perspire', 'persuade', 'pertains', 'perturbs', 'perusing', 'perusals', 'peruvian', 'pervaded', 'pervades', 'perverse', 'perverts', 'pervious', 'peskiest', 'pestered', 'pestling', 'petcocks', 'petering', 'petioles', 'petition', 'pettiest', 'pettifog', 'petulant', 'petunias', 'phaetons', 'phantasm', 'phantoms', 'pharaohs', 'pharisee', 'pharmacy', 'pheasant', 'philters', 'phoniest', 'phonemes', 'phonemic', 'phonetic', 'phosgene', 'phosphor', 'photonic', 'phrasing', 'phrasals', 'physical', 'physique', 'pianists', 'picadors', 'picayune', 'piccolos', 'pickings', 'pickaxed', 'pickaxes', 'pickerel', 'picketed', 'pickling', 'picklock', 'pictured', 'pictures', 'piddling', 'piebalds', 'piedmont', 'piercing', 'pietisms', 'piffling', 'pigments', 'pigskins', 'pigsties', 'pigtails', 'pilaster', 'pilchard', 'pilfered', 'pilgrims', 'pillions', 'pillaged', 'pillages', 'pillowed', 'piloting', 'pimentos', 'pimiento', 'pimpling', 'pinafore', 'pinballs', 'pinching', 'pinheads', 'pinholes', 'pinkeyes', 'pinnaces', 'pinnacle', 'pinochle', 'pinpoint', 'pinprick', 'pinscher', 'pintsize', 'pinwales', 'pinwheel', 'pinworms', 'pioneers', 'pipeline', 'pipettes', 'piquancy', 'piracies', 'piranhas', 'pirating', 'pitapats', 'pitching', 'pitchers', 'pitchout', 'pitfalls', 'pithiest', 'pitiably', 'pitiable', 'pitiless', 'pittance', 'pivoting', 'pizzeria', 'placards', 'placated', 'placates', 'placebos', 'placenta', 'placidly', 'plackets', 'plaguing', 'plainest', 'plaiting', 'plangent', 'planking', 'plankton', 'planning', 'planners', 'planting', 'planters', 'plantain', 'plashing', 'plasters', 'plastics', 'plastids', 'plastron', 'plateaus', 'plateful', 'platelet', 'platform', 'platinum', 'platonic', 'platoons', 'platting', 'platters', 'platypus', 'plaudits', 'playable', 'playback', 'playbill', 'playboys', 'playgoer', 'playmate', 'playpens', 'playroom', 'pleading', 'pleasing', 'pleasant', 'pleasure', 'pleating', 'plebeian', 'plectrum', 'pledging', 'pledgees', 'plenties', 'pleonasm', 'plethora', 'pleurisy', 'plicated', 'plicates', 'plighted', 'pliocene', 'plodding', 'plodders', 'plopping', 'plotting', 'plotters', 'plucking', 'pluckily', 'pluckier', 'plugging', 'pluggers', 'plumages', 'plumbing', 'plumbers', 'plummets', 'plumping', 'plumpest', 'plunders', 'plunging', 'plungers', 'plunking', 'pluribus', 'plushest', 'plymouth', 'plywoods', 'poaching', 'poachers', 'pocketed', 'pockmark', 'podiatry', 'poetries', 'poignant', 'pointing', 'pointers', 'poisoned', 'pokeweed', 'pokiness', 'polarity', 'polarize', 'polaroid', 'poleaxes', 'polecats', 'polemics', 'polestar', 'policing', 'policies', 'polished', 'polishes', 'polities', 'politest', 'politely', 'politics', 'politick', 'politico', 'polkaing', 'polliwog', 'pollster', 'polluted', 'polluter', 'pollutes', 'pollywog', 'polonium', 'poltroon', 'polygamy', 'polyglot', 'polygons', 'polymers', 'pomading', 'pomander', 'pommeled', 'pondered', 'poniards', 'pontiffs', 'pontoons', 'ponytail', 'poolroom', 'poperies', 'popinjay', 'popovers', 'populist', 'populism', 'populous', 'populace', 'populate', 'porkpies', 'porosity', 'porphyry', 'porpoise', 'porridge', 'portions', 'portably', 'portable', 'portaged', 'portages', 'portends', 'portents', 'porthole', 'portiere', 'portlier', 'portland', 'portrait', 'portrays', 'portugal', 'positing', 'position', 'positive', 'positron', 'possibly', 'possible', 'postings', 'postages', 'postcard', 'postdate', 'posterns', 'postmark', 'postpaid', 'postpone', 'postured', 'postures', 'postural', 'potables', 'potashes', 'potation', 'potatoes', 'potbelly', 'pothered', 'potherbs', 'potholes', 'pothooks', 'potlatch', 'potlucks', 'potsherd', 'pottiest', 'pottages', 'pottered', 'pouching', 'poultice', 'pouncing', 'pounding', 'poundage', 'powdered', 'powering', 'powerful', 'powwowed', 'practice', 'praetors', 'prairies', 'praising', 'pralines', 'prancing', 'pranking', 'pratfall', 'prattles', 'prattled', 'preached', 'preacher', 'preaches', 'preamble', 'preceded', 'precedes', 'precepts', 'precinct', 'precious', 'preclude', 'precooks', 'predated', 'predates', 'predator', 'predicts', 'preening', 'prefaced', 'prefacer', 'prefaces', 'prefects', 'prefixed', 'prefixes', 'pregnant', 'preheats', 'prejudge', 'prelates', 'preluded', 'preludes', 'premiers', 'premiere', 'premised', 'premises', 'premiums', 'premolar', 'prenatal', 'prepares', 'prepared', 'prepping', 'preppies', 'prepuces', 'presaged', 'presages', 'presence', 'presents', 'preserve', 'presided', 'presides', 'presoaks', 'presorts', 'pressing', 'pressers', 'pressmen', 'pressman', 'pressrun', 'pressure', 'prestige', 'presumes', 'presumed', 'preteens', 'pretends', 'pretense', 'preterit', 'pretests', 'pretexts', 'pretrial', 'prettied', 'prettier', 'pretties', 'prettily', 'prettify', 'pretzels', 'prevails', 'prevents', 'previous', 'previews', 'pricking', 'prickles', 'prickled', 'priedieu', 'priestly', 'priggish', 'primates', 'primeval', 'primmest', 'primping', 'primrose', 'princely', 'princess', 'principe', 'printing', 'printers', 'printout', 'priories', 'priority', 'prioress', 'prisoner', 'prissier', 'pristine', 'privates', 'probably', 'probable', 'probated', 'probates', 'problems', 'proceeds', 'proclaim', 'proctors', 'procured', 'procurer', 'procures', 'prodding', 'prodders', 'prodigal', 'produced', 'producer', 'produces', 'products', 'profaned', 'profanes', 'proffers', 'profiles', 'profiled', 'profited', 'profound', 'profundo', 'programs', 'progress', 'prohibit', 'projects', 'prolific', 'prologue', 'prolongs', 'promised', 'promises', 'promoted', 'promoter', 'promotes', 'prompted', 'prompter', 'promptly', 'pronging', 'pronouns', 'proofing', 'propanes', 'properly', 'property', 'prophase', 'prophecy', 'prophesy', 'prophets', 'proposed', 'proposes', 'proposal', 'propound', 'propping', 'prorated', 'prorates', 'prorogue', 'prosodic', 'prospect', 'prospers', 'prostate', 'protects', 'proteges', 'proteins', 'protests', 'protocol', 'protozoa', 'protract', 'protrude', 'proudest', 'provably', 'provable', 'proverbs', 'province', 'provided', 'provides', 'provisos', 'provoked', 'provokes', 'provosts', 'prowling', 'prowlers', 'proximal', 'prudence', 'prurient', 'prussian', 'psalmist', 'psalmody', 'psaltery', 'psychics', 'ptomaine', 'ptyalins', 'publicly', 'publican', 'puckered', 'puddings', 'puddling', 'pudgiest', 'puffiest', 'puffball', 'pugilism', 'pugilist', 'puissant', 'pulitzer', 'pullback', 'pullouts', 'pullover', 'pulpiest', 'pulpwood', 'pulsated', 'pulsates', 'pummeled', 'pumpkins', 'punching', 'punchier', 'puncheon', 'punctual', 'puncture', 'pungency', 'puniness', 'punished', 'punishes', 'punitive', 'punsters', 'pupating', 'pupation', 'puppetry', 'purblind', 'purchase', 'pureness', 'purebred', 'pureeing', 'purified', 'purifier', 'purifies', 'purities', 'puritans', 'purlieus', 'purloins', 'purpling', 'purplish', 'purports', 'purposed', 'purposes', 'pursuing', 'pursuers', 'pursuant', 'pursuits', 'purulent', 'purveyed', 'purveyor', 'purviews', 'pushiest', 'pushcart', 'pushover', 'pushpins', 'pustules', 'putative', 'puttered', 'puttying', 'puzzling', 'puzzlers', 'pyorrhea', 'pyramids', 'pyridine', 'quaalude', 'quacking', 'quackery', 'quadrant', 'quadrats', 'quadrate', 'quadroon', 'quaffing', 'quagmire', 'quailing', 'quainter', 'quandary', 'quantity', 'quantify', 'quarrels', 'quarried', 'quarries', 'quarters', 'quartern', 'quartets', 'quartzes', 'quashing', 'quatrain', 'quavered', 'queasier', 'queening', 'queering', 'queerest', 'quelling', 'quenched', 'quencher', 'quenches', 'quenelle', 'querying', 'questing', 'question', 'quibbles', 'quibbled', 'quickies', 'quickest', 'quickens', 'quieting', 'quietest', 'quietude', 'quilling', 'quilting', 'quinines', 'quinsies', 'quintals', 'quintets', 'quipping', 'quipster', 'quirkier', 'quisling', 'quitrent', 'quitting', 'quitters', 'quivered', 'quixotic', 'quizzing', 'quoining', 'quotably', 'quotable', 'quotient', 'rabidity', 'raccoons', 'raceways', 'racially', 'racquets', 'radiance', 'radiated', 'radiates', 'radiator', 'radicals', 'radicand', 'radioing', 'radishes', 'radiuses', 'raffling', 'ragtimes', 'ragweeds', 'railings', 'raillery', 'railroad', 'railways', 'raiments', 'rainiest', 'rainbows', 'raincoat', 'raindrop', 'rainfall', 'rainless', 'rainwear', 'rakishly', 'rallying', 'rambling', 'ramblers', 'ramekins', 'ramified', 'ramifies', 'rampaged', 'rampages', 'rampancy', 'ramparts', 'ranching', 'ranchers', 'ranchero', 'randomly', 'rangiest', 'rankling', 'ransacks', 'ransomed', 'rapacity', 'rapidity', 'rapidest', 'rappings', 'rapports', 'raptures', 'rarefied', 'rarefies', 'rarities', 'rashness', 'raspiest', 'ratchets', 'ratified', 'ratifies', 'rationed', 'rational', 'ratlines', 'rattiest', 'rattaned', 'rattling', 'rattlers', 'ravaging', 'raveling', 'ravening', 'ravenous', 'ravished', 'ravishes', 'rawboned', 'rawhides', 'reaching', 'reacting', 'reaction', 'reactive', 'reactors', 'reactant', 'readiest', 'readably', 'readable', 'readings', 'readying', 'readjust', 'reaffirm', 'reagents', 'realists', 'realisms', 'realness', 'realigns', 'realized', 'realizes', 'realties', 'realtors', 'reappear', 'rearming', 'rearmost', 'rearview', 'rearward', 'reasoned', 'reassess', 'reassign', 'reassume', 'reassure', 'reattach', 'reawaken', 'rebating', 'rebelled', 'rebirths', 'rebooted', 'rebounds', 'rebuffed', 'rebuilds', 'rebuking', 'rebutted', 'rebutter', 'rebuttal', 'recalled', 'recanted', 'recapped', 'receding', 'receipts', 'received', 'receiver', 'receives', 'recently', 'receptor', 'recessed', 'recesses', 'recharge', 'rechecks', 'reciting', 'recitals', 'reckless', 'reckoned', 'reclaims', 'reclined', 'reclines', 'recluses', 'recoiled', 'recorded', 'recorder', 'recounts', 'recouped', 'recourse', 'recovers', 'recovery', 'recreant', 'recreate', 'recruits', 'recurred', 'recycled', 'recycles', 'redacted', 'redactor', 'redbirds', 'redcoats', 'reddened', 'redeemed', 'redeemer', 'redefine', 'redheads', 'redirect', 'rednecks', 'redolent', 'redouble', 'redounds', 'redstart', 'reducing', 'redwings', 'redwoods', 'reediest', 'refereed', 'referees', 'referent', 'referred', 'referrer', 'referral', 'refilled', 'refining', 'refiners', 'refinery', 'refinish', 'refitted', 'reflects', 'reflexes', 'reflexly', 'reforest', 'reformed', 'reformer', 'reformat', 'refracts', 'refrains', 'refueled', 'refugees', 'refunded', 'refusing', 'refusals', 'refuting', 'refuters', 'regained', 'regaling', 'regalias', 'regarded', 'regattas', 'regicide', 'regimens', 'regiment', 'regional', 'register', 'registry', 'regroups', 'regulars', 'regulate', 'rehashed', 'rehashes', 'rehearse', 'rehoused', 'rehouses', 'reigning', 'reindeer', 'reinsert', 'reinsure', 'reinvent', 'reinvest', 'reissued', 'reissues', 'rejected', 'rejecter', 'rejoiced', 'rejoices', 'rejoined', 'rekindle', 'relapsed', 'relapses', 'relating', 'relaters', 'relation', 'relative', 'relaxing', 'relaxant', 'relaying', 'released', 'releases', 'relegate', 'relented', 'relevant', 'reliably', 'reliable', 'reliance', 'relieved', 'relieves', 'religion', 'relining', 'relished', 'relishes', 'reliving', 'reloaded', 'relocate', 'remained', 'remaking', 'remanded', 'remarked', 'remedied', 'remedies', 'remedial', 'remember', 'reminded', 'reminder', 'remitted', 'remitter', 'remittal', 'remnants', 'remodels', 'remotest', 'remotely', 'remounts', 'removing', 'removers', 'removals', 'renaming', 'rendered', 'reneging', 'renegade', 'renewing', 'renewals', 'renitent', 'renounce', 'renovate', 'renowned', 'renumber', 'reoccupy', 'reoccurs', 'reopened', 'reorders', 'repaired', 'repartee', 'repaying', 'repealed', 'repeater', 'repeated', 'repelled', 'repented', 'rephrase', 'repining', 'replying', 'replaced', 'replaces', 'replants', 'replayed', 'replicas', 'reporter', 'reported', 'reposing', 'reposals', 'reprieve', 'reprints', 'reprised', 'reprises', 'reprisal', 'reproach', 'reproofs', 'reproved', 'reproves', 'reptiles', 'republic', 'repulsed', 'repulses', 'reputing', 'requests', 'requiems', 'required', 'requires', 'requited', 'requites', 'requital', 'rerouted', 'reroutes', 'rescinds', 'rescuing', 'rescuers', 'research', 'reseller', 'resemble', 'resented', 'reserved', 'reserves', 'residing', 'resident', 'residues', 'residual', 'resigned', 'resinous', 'resisted', 'resister', 'resistor', 'resolute', 'resolves', 'resolved', 'resonant', 'resonate', 'resorted', 'resounds', 'resource', 'respects', 'respells', 'respired', 'respires', 'respites', 'responds', 'response', 'restarts', 'restated', 'restates', 'restless', 'restocks', 'restored', 'restorer', 'restores', 'restrain', 'restrict', 'resulted', 'resuming', 'resurvey', 'retables', 'retailed', 'retailer', 'retained', 'retainer', 'retaking', 'retarded', 'retching', 'rethinks', 'reticent', 'reticule', 'retinues', 'retiring', 'retirees', 'retooled', 'retorted', 'retraced', 'retraces', 'retracts', 'retrains', 'retreads', 'retreats', 'retrench', 'retrials', 'retrieve', 'retrofit', 'returned', 'returnee', 'retyping', 'reunions', 'reunited', 'reunites', 'reusable', 'revamped', 'revealed', 'reveille', 'reveling', 'revelers', 'revenant', 'revenged', 'revenges', 'revenues', 'revering', 'reveries', 'reverend', 'reverent', 'reversed', 'reverses', 'reversal', 'reverted', 'reviewed', 'reviewer', 'reviling', 'revising', 'revisers', 'revision', 'revisits', 'reviving', 'revivers', 'revivals', 'revivify', 'revoking', 'revolted', 'revolved', 'revolver', 'revolves', 'rewarded', 'rewiring', 'reworded', 'reworked', 'rewriter', 'rewrites', 'rezoning', 'rheniums', 'rheostat', 'rhesuses', 'rhetoric', 'rhizoids', 'rhizomes', 'rhodesia', 'rhomboid', 'rhubarbs', 'rhythmic', 'ribaldry', 'ribbings', 'ribboned', 'ribosome', 'richness', 'richmond', 'rickrack', 'ricochet', 'riddance', 'riddling', 'ridicule', 'riffling', 'riffraff', 'rigatoni', 'riggings', 'righting', 'rightist', 'rightful', 'rigidity', 'rigorous', 'ringlets', 'ringside', 'ringtail', 'ringworm', 'riparian', 'ripcords', 'ripening', 'riposted', 'ripostes', 'rippling', 'riskiest', 'rissoles', 'ritziest', 'rivaling', 'riverbed', 'riveting', 'rivulets', 'roadbeds', 'roadside', 'roadster', 'roadways', 'roadwork', 'roasting', 'roasters', 'robotics', 'rockiest', 'rocketed', 'rocketry', 'rockfish', 'roebucks', 'roentgen', 'rogation', 'roisters', 'rollaway', 'rollback', 'rollicks', 'romances', 'romanced', 'romantic', 'roomiest', 'roomette', 'roomfuls', 'roommate', 'roosting', 'roosters', 'rootless', 'rosaries', 'rosebuds', 'rosebush', 'rosemary', 'rosettes', 'rosewood', 'rostrums', 'rotaries', 'rotarian', 'rotating', 'rotation', 'rotators', 'rotatory', 'rotifers', 'rottener', 'rotundas', 'roughing', 'roughest', 'roughage', 'roughens', 'roughhew', 'roulades', 'roulette', 'rounding', 'roundest', 'roundels', 'roundups', 'rousting', 'routines', 'rowboats', 'rowdiest', 'royalist', 'rubbings', 'rubdowns', 'rubicund', 'rubidium', 'rucksack', 'ruckuses', 'ructions', 'ruddiest', 'rudeness', 'rudiment', 'ruefully', 'ruffians', 'ruffling', 'ruggedly', 'rumanian', 'rumbaing', 'rumbling', 'ruminant', 'ruminate', 'rummaged', 'rummages', 'rumoring', 'rumpling', 'rumpuses', 'runabout', 'runaways', 'runbacks', 'runniest', 'ruptured', 'ruptures', 'russians', 'rustiest', 'rustling', 'rustlers', 'rutabaga', 'ruthless', 'sabbaths', 'sabotage', 'saboteur', 'sacristy', 'saddened', 'saddling', 'saddlers', 'sadirons', 'sadistic', 'safeness', 'safeties', 'saffrons', 'sagacity', 'sageness', 'saguaros', 'sailboat', 'sailfish', 'sailorly', 'sainting', 'salaamed', 'salacity', 'salaried', 'salaries', 'salesmen', 'salesman', 'salience', 'salients', 'salinity', 'salivary', 'salivate', 'sallying', 'sallower', 'saltiest', 'saltines', 'saluting', 'salutary', 'salvable', 'salvador', 'salvaged', 'salvager', 'salvages', 'samarium', 'sambaing', 'sameness', 'samisens', 'samovars', 'samoyeds', 'sampling', 'samplers', 'sanctity', 'sanctify', 'sanction', 'sanctums', 'sandiest', 'sandarac', 'sandbags', 'sandbars', 'sandlots', 'sandwich', 'saneness', 'sanguine', 'sanitary', 'sanitize', 'sanskrit', 'santiago', 'sapience', 'saplings', 'sappiest', 'sapphire', 'sapwoods', 'saracens', 'sarcasms', 'sarcomas', 'sardined', 'sardines', 'sardinia', 'sardonic', 'sargasso', 'sashayed', 'sassiest', 'satanist', 'satanism', 'satchels', 'satiable', 'satiated', 'satiates', 'satirist', 'satirize', 'saturate', 'saturday', 'sauciest', 'saucepan', 'saunters', 'saurians', 'sausages', 'sauteing', 'sauterne', 'savaging', 'savagely', 'savagery', 'savannas', 'savoring', 'savories', 'savvying', 'sawbones', 'sawbucks', 'sawhorse', 'sawmills', 'saxhorns', 'scabbing', 'scabbier', 'scabbard', 'scabrous', 'scaffold', 'scaliest', 'scalawag', 'scalding', 'scallion', 'scallops', 'scalping', 'scalpers', 'scalpels', 'scamping', 'scampers', 'scandals', 'scandium', 'scanning', 'scanners', 'scansion', 'scanting', 'scantier', 'scantest', 'scapulas', 'scapular', 'scariest', 'scarcity', 'scarcest', 'scarcely', 'scarring', 'scathing', 'scatting', 'scatters', 'scavenge', 'scenario', 'scenting', 'scepters', 'schedule', 'scheming', 'schemers', 'schemata', 'scherzos', 'schizoid', 'schmaltz', 'schnapps', 'schnooks', 'scholars', 'schooled', 'schooner', 'schussed', 'schusses', 'sciatica', 'sciences', 'scimitar', 'scission', 'scissors', 'scissile', 'scoffing', 'scofflaw', 'scolding', 'scooping', 'scooting', 'scooters', 'scorched', 'scorches', 'scorning', 'scornful', 'scorpion', 'scotched', 'scotches', 'scotland', 'scotsmen', 'scotsman', 'scotties', 'scottish', 'scouring', 'scourged', 'scourges', 'scouting', 'scowling', 'scrabble', 'scraggly', 'scramble', 'scrammed', 'scraping', 'scrapers', 'scrapped', 'scrapper', 'scrapple', 'scratchy', 'scrawled', 'screamed', 'screamer', 'screened', 'screwing', 'screwier', 'scribing', 'scribers', 'scribble', 'scrimped', 'scripted', 'scrofula', 'scrolled', 'scrounge', 'scrubbed', 'scrubber', 'scruples', 'scrupled', 'scrutiny', 'scudding', 'scuffing', 'scuffles', 'scuffled', 'sculling', 'scullion', 'scullery', 'sculpted', 'sculptor', 'scumming', 'scuppers', 'scurried', 'scurries', 'scurvies', 'scuttles', 'scuttled', 'scything', 'seaboard', 'seaborne', 'seacoast', 'seafarer', 'seafoods', 'seagoing', 'sealskin', 'sealyham', 'seamiest', 'seamless', 'seaplane', 'seaports', 'searched', 'searcher', 'searches', 'seascape', 'seashell', 'seashore', 'seasides', 'seasoned', 'seasonal', 'seaweeds', 'seceding', 'secluded', 'secludes', 'seconded', 'secondly', 'secreted', 'secretes', 'secretly', 'secretor', 'sections', 'sectored', 'seculars', 'securing', 'securers', 'security', 'securest', 'securely', 'sedating', 'sedation', 'sedately', 'sedative', 'sediment', 'sedition', 'seducing', 'seducers', 'sedulity', 'sedulous', 'seediest', 'seedings', 'seedcase', 'seedling', 'seedless', 'seemlier', 'seepages', 'seesawed', 'seething', 'segments', 'seignior', 'seizable', 'seizures', 'selected', 'selector', 'selenium', 'selfhood', 'selfless', 'selfsame', 'sellouts', 'seltzers', 'selvages', 'semantic', 'semester', 'seminars', 'seminary', 'semitone', 'semolina', 'senators', 'sendoffs', 'senesced', 'senesces', 'senility', 'sensibly', 'sensible', 'sensuous', 'sentence', 'sentient', 'sentinel', 'sentries', 'separate', 'sequence', 'sequoias', 'seraglio', 'seraphic', 'seraphim', 'serenity', 'serenest', 'serenade', 'serenely', 'serfdoms', 'sergeant', 'seriatim', 'serology', 'serpents', 'serrated', 'serrates', 'servings', 'servants', 'serviced', 'services', 'servitor', 'sessions', 'setbacks', 'settings', 'settling', 'settlers', 'sevenths', 'severing', 'severity', 'severest', 'severely', 'sewerage', 'sexiness', 'sexology', 'sextants', 'sextuple', 'sexually', 'shabbier', 'shacking', 'shackles', 'shackled', 'shadiest', 'shadings', 'shadowed', 'shafting', 'shagbark', 'shagging', 'shaggier', 'shakiest', 'shakable', 'shakings', 'shakeups', 'shallots', 'shallows', 'shambles', 'shambled', 'shameful', 'shamming', 'shampoos', 'shamrock', 'shamuses', 'shanghai', 'shanties', 'shantung', 'shapeups', 'sharings', 'sharpers', 'sharpies', 'sharpest', 'sharpens', 'shatters', 'shavings', 'shavians', 'shearing', 'sheathed', 'sheathes', 'sheaving', 'shebangs', 'shedding', 'shedders', 'sheepish', 'sheering', 'sheerest', 'sheeting', 'sheikdom', 'shelling', 'shellacs', 'shelters', 'shelties', 'shelving', 'shepherd', 'sheraton', 'sherbets', 'sheriffs', 'sherries', 'shetland', 'shielded', 'shifting', 'shiftily', 'shiftier', 'shilling', 'shimming', 'shimmers', 'shimmies', 'shimmied', 'shiniest', 'shinbone', 'shindigs', 'shingles', 'shingled', 'shinning', 'shinnies', 'shinnied', 'shipment', 'shipload', 'shipmate', 'shipping', 'shippers', 'shipyard', 'shirking', 'shirring', 'shirting', 'shivered', 'shoaling', 'shocking', 'shoddily', 'shoddier', 'shoddies', 'shoehorn', 'shoelace', 'shoeless', 'shoetree', 'shooting', 'shooters', 'shopping', 'shoppers', 'shoptalk', 'shopworn', 'shorings', 'shorting', 'shortest', 'shortage', 'shortcut', 'shortens', 'shotguns', 'shoulder', 'shouting', 'shouters', 'shoveled', 'showiest', 'showings', 'showered', 'showboat', 'showcase', 'showdown', 'showgirl', 'showoffs', 'shrapnel', 'shredded', 'shredder', 'shrewder', 'shrewdly', 'shrewish', 'shrieked', 'shrilled', 'shriller', 'shrining', 'shriving', 'shrivels', 'shrouded', 'shrugged', 'shrunken', 'shucking', 'shudders', 'shuffles', 'shuffled', 'shunning', 'shunners', 'shunpike', 'shunting', 'shushing', 'shutdown', 'shuteyes', 'shutoffs', 'shutouts', 'shutting', 'shutters', 'shuttles', 'shuttled', 'shylocks', 'shysters', 'siberian', 'sibilant', 'siblings', 'sicilian', 'sickbays', 'sickbeds', 'sickened', 'sickling', 'sicklier', 'sickness', 'sickroom', 'sidecars', 'sidekick', 'sideline', 'sidelong', 'sidereal', 'sidestep', 'sidewalk', 'sideward', 'sideways', 'siftings', 'sighting', 'signally', 'signaled', 'signaler', 'signeted', 'signpost', 'silences', 'silenced', 'silencer', 'silently', 'silicate', 'silicons', 'silicone', 'silkiest', 'silkworm', 'silliest', 'silurian', 'silvered', 'simmered', 'simonize', 'simpered', 'simplest', 'simplify', 'simulate', 'sincerer', 'sinecure', 'sinfully', 'singeing', 'singling', 'singsong', 'singular', 'sinister', 'sinkable', 'sinkhole', 'sinology', 'siphoned', 'siphonal', 'sirloins', 'siroccos', 'sisyphus', 'sittings', 'situated', 'situates', 'sixpence', 'sixpenny', 'sixteens', 'sixtieth', 'sizzling', 'skeletal', 'skeleton', 'skeptics', 'sketched', 'sketches', 'skewered', 'skewbald', 'skidding', 'skidooed', 'skillets', 'skillful', 'skimming', 'skimmers', 'skimping', 'skimpily', 'skimpier', 'skinless', 'skinning', 'skinners', 'skinnier', 'skipping', 'skippers', 'skirling', 'skirmish', 'skirting', 'skitters', 'skittles', 'skittish', 'skivvies', 'skulking', 'skullcap', 'skunking', 'skydived', 'skydiver', 'skydives', 'skyjacks', 'skylarks', 'skylight', 'skylines', 'slabbing', 'slacking', 'slackers', 'slackest', 'slackens', 'slagging', 'slamming', 'slanders', 'slanting', 'slapdash', 'slapping', 'slashing', 'slatings', 'slathers', 'slatting', 'slattern', 'slavered', 'slayings', 'sleazier', 'sledding', 'sledging', 'sleeking', 'sleekest', 'sleeping', 'sleepers', 'sleepily', 'sleepier', 'sleeting', 'sleeving', 'sleighed', 'sleights', 'sleuthed', 'slicking', 'slickers', 'slickest', 'slighted', 'slighter', 'slightly', 'slimiest', 'slimming', 'slimmest', 'slinging', 'slinking', 'slinkier', 'slipcase', 'slipknot', 'slipover', 'slipping', 'slippage', 'slippers', 'slippery', 'slipshod', 'slithers', 'slithery', 'slitting', 'slivered', 'slobbers', 'slobbery', 'slogging', 'slopping', 'sloppily', 'sloppier', 'sloshing', 'slothful', 'slotting', 'slouched', 'slouches', 'sloughed', 'slovenes', 'slovenly', 'slowness', 'slowdown', 'slowpoke', 'sludging', 'slugabed', 'slugfest', 'slugging', 'sluggers', 'sluggard', 'sluggish', 'sluicing', 'slumbers', 'slumlord', 'slumming', 'slumping', 'slurping', 'slurring', 'slushing', 'slushier', 'sluttish', 'smacking', 'smackers', 'smallest', 'smallish', 'smallpox', 'smarting', 'smartest', 'smartens', 'smashing', 'smashups', 'smatters', 'smearing', 'smearier', 'smelling', 'smellier', 'smelting', 'smelters', 'smidgens', 'smilaxes', 'smirched', 'smirches', 'smirking', 'smithies', 'smocking', 'smoggier', 'smokiest', 'smolders', 'smooched', 'smooches', 'smoothed', 'smoother', 'smoothes', 'smoothly', 'smoothen', 'smothers', 'smudging', 'smudgily', 'smudgier', 'smuggest', 'smuggles', 'smuggled', 'smuggler', 'smutting', 'smuttily', 'smuttier', 'snacking', 'snaffles', 'snaffled', 'snafuing', 'snagging', 'snakiest', 'snapping', 'snappers', 'snappily', 'snappier', 'snappish', 'snapshot', 'snarling', 'snatched', 'snatcher', 'snatches', 'snazzier', 'sneaking', 'sneakers', 'sneakily', 'sneakier', 'sneering', 'sneezing', 'snickers', 'sniffing', 'sniffles', 'sniffled', 'snifters', 'sniggers', 'snipping', 'snippily', 'snippier', 'snippets', 'snippety', 'snitched', 'snitcher', 'snitches', 'sniveled', 'snobbism', 'snobbery', 'snobbish', 'snookers', 'snooping', 'snoopily', 'snoopier', 'snootily', 'snootier', 'snoozing', 'snorkels', 'snorting', 'snowiest', 'snowball', 'snowbird', 'snowcaps', 'snowdrop', 'snowfall', 'snowplow', 'snowshoe', 'snowsuit', 'snubbing', 'snubbers', 'snuffing', 'snuffles', 'snuffbox', 'snuffled', 'snuggest', 'snuggles', 'snuggery', 'snuggled', 'soakages', 'soapiest', 'soapsuds', 'soapwort', 'sobering', 'soberest', 'sobriety', 'sociably', 'sociable', 'socially', 'societal', 'sockeyes', 'socratic', 'sodality', 'soddened', 'soddenly', 'sodomies', 'softness', 'softball', 'softened', 'softener', 'software', 'softwood', 'soggiest', 'soilages', 'sojourns', 'solacing', 'solarium', 'soldered', 'soldiers', 'solecism', 'solemner', 'solemnly', 'solenoid', 'solicits', 'solidity', 'solidest', 'solidify', 'solitary', 'solitude', 'soloists', 'solstice', 'solution', 'solvable', 'solvency', 'solvents', 'sombrero', 'somebody', 'sometime', 'somewhat', 'songbird', 'songfest', 'songster', 'sonority', 'sonorous', 'sootiest', 'soothing', 'sophists', 'sophisms', 'soppiest', 'sopranos', 'sorcerer', 'soreness', 'sorehead', 'sorghums', 'sorority', 'sorriest', 'sorrowed', 'souffles', 'soughing', 'sounding', 'sounders', 'soundest', 'soupiest', 'sourpuss', 'southers', 'southern', 'southpaw', 'souvenir', 'soybeans', 'spacings', 'spacebar', 'spacemen', 'spaceman', 'spacious', 'spackles', 'spackled', 'spadices', 'spangles', 'spangled', 'spaniard', 'spaniels', 'spanking', 'spankers', 'spanning', 'spanners', 'sparking', 'sparkles', 'sparkled', 'sparkler', 'sparring', 'sparrows', 'sparsest', 'sparsely', 'spastics', 'spatting', 'spatters', 'spatulas', 'spatular', 'spavined', 'spawning', 'speaking', 'speakers', 'spearing', 'specials', 'specific', 'specimen', 'specious', 'specking', 'speckles', 'speckled', 'specters', 'spectral', 'spectrum', 'speeches', 'speeding', 'speeders', 'speedily', 'speedier', 'speedups', 'speedway', 'spelling', 'spellers', 'spending', 'spenders', 'sphagnum', 'sphenoid', 'spheroid', 'sphinxes', 'spiciest', 'spicules', 'spicular', 'spiffily', 'spiffier', 'spikiest', 'spilling', 'spillage', 'spillway', 'spiniest', 'spindles', 'spindled', 'spinning', 'spinners', 'spinster', 'spiracle', 'spiraled', 'spirilla', 'spirited', 'spiteful', 'spirfire', 'spitting', 'spittles', 'spittoon', 'splashed', 'splashes', 'splatter', 'splaying', 'splendor', 'splendid', 'splicing', 'splinted', 'splinter', 'splotchy', 'splurged', 'splurges', 'splutter', 'spoiling', 'spoilage', 'spondaic', 'spondees', 'sponging', 'spongers', 'spongier', 'sponsors', 'spoofing', 'spooking', 'spookily', 'spookier', 'spooling', 'spooning', 'spoonful', 'sporadic', 'sporrans', 'sportily', 'sportier', 'sportive', 'sporting', 'spotless', 'spotting', 'spotters', 'spottier', 'spousals', 'spouting', 'sprained', 'sprawled', 'spraying', 'sprayers', 'spreader', 'sprigged', 'sprinkle', 'sprinted', 'sprinter', 'sprocket', 'sprouted', 'sprucing', 'sprucest', 'spunkily', 'spunkier', 'spurious', 'spurning', 'spurring', 'spurting', 'sputniks', 'sputters', 'spyglass', 'squabble', 'squadron', 'squalors', 'squalled', 'squander', 'squaring', 'squarest', 'squarely', 'squashed', 'squashes', 'squatted', 'squatter', 'squawked', 'squeaked', 'squealed', 'squealer', 'squeegee', 'squeezed', 'squeezes', 'squiggle', 'squinted', 'squiring', 'squirmed', 'squirrel', 'squirted', 'squished', 'squishes', 'stabbing', 'stabiles', 'stabling', 'stablest', 'staccato', 'stacking', 'stadiums', 'staffing', 'stagiest', 'staggers', 'stagnant', 'stagnate', 'staidest', 'staining', 'stairway', 'stalking', 'stalkers', 'stalling', 'stallion', 'stalwart', 'stammers', 'stamping', 'stampede', 'stanched', 'stanches', 'standing', 'standard', 'standbys', 'standees', 'standoff', 'standout', 'stannous', 'snapling', 'staplers', 'starched', 'starches', 'stardoms', 'starfish', 'stargaze', 'starkest', 'starling', 'starlets', 'starring', 'starrier', 'starting', 'starters', 'startles', 'startled', 'starving', 'stashing', 'stations', 'statuary', 'statures', 'statuses', 'statutes', 'steadily', 'steadier', 'steadies', 'steadied', 'stealing', 'stealers', 'stealths', 'stealthy', 'steaming', 'steamers', 'steamily', 'steamier', 'steatite', 'steeling', 'steelier', 'steenbok', 'steeping', 'steepest', 'steeples', 'steepens', 'steering', 'steerage', 'stemless', 'stemming', 'stemware', 'stenches', 'stencils', 'stepping', 'steppers', 'stepsons', 'stepwise', 'sterling', 'sternest', 'steroids', 'stetsons', 'stetting', 'stewards', 'stibnite', 'sticking', 'stickers', 'stickier', 'stickles', 'stickled', 'stickler', 'stickpin', 'stickups', 'stiffing', 'stiffest', 'stiffens', 'stifling', 'stigmata', 'stiletto', 'stilling', 'stillest', 'stimulus', 'stinging', 'stingers', 'stingier', 'stingray', 'stinking', 'stinkers', 'stinkpot', 'stinting', 'stipends', 'stipples', 'stippled', 'stirrers', 'stirring', 'stirrups', 'stitched', 'stitches', 'stocking', 'stockier', 'stockade', 'stockmen', 'stockman', 'stodgily', 'stodgier', 'stoicism', 'stolidly', 'stomachs', 'stomping', 'stoniest', 'stooping', 'stopcock', 'stopgaps', 'stopover', 'stopping', 'stoppers', 'stoppage', 'storages', 'storming', 'stormily', 'stormier', 'stoutest', 'stowages', 'stowaway', 'straddle', 'strafing', 'straggly', 'straggle', 'straight', 'strained', 'strainer', 'straiten', 'stranded', 'stranger', 'strangle', 'strapped', 'strategy', 'stratify', 'straying', 'streaked', 'streamed', 'streamer', 'strength', 'stressed', 'stresses', 'stretchy', 'streusel', 'strewing', 'striated', 'striates', 'stricken', 'stricter', 'strictly', 'stridden', 'strident', 'striding', 'strikers', 'striking', 'stringed', 'stringer', 'striping', 'stripers', 'stripped', 'stripper', 'striving', 'stroking', 'strolled', 'stroller', 'stronger', 'strongly', 'strophes', 'stropped', 'strudels', 'struggle', 'strummed', 'strummer', 'strumpet', 'strutted', 'stubbing', 'stubbier', 'stubbles', 'stubborn', 'stuccoes', 'studying', 'studbook', 'studding', 'students', 'studious', 'stuffing', 'stuffily', 'stuffier', 'stultify', 'stumbles', 'stumbled', 'stumping', 'stunners', 'stunning', 'stunting', 'stupider', 'stupidly', 'sturdily', 'sturdier', 'sturgeon', 'stutters', 'stylings', 'stylists', 'stylized', 'stylizes', 'styluses', 'stymying', 'styptics', 'styrenes', 'subacute', 'subclass', 'subduing', 'subgenus', 'subgroup', 'subheads', 'subhuman', 'subjects', 'subjoins', 'sublease', 'sublimed', 'sublimer', 'sublimes', 'submerge', 'submerse', 'suborder', 'suborned', 'subplots', 'subpoena', 'subsists', 'subsided', 'subsides', 'subsoils', 'subsonic', 'subsumed', 'subsumes', 'subtends', 'subtitle', 'subtlest', 'subtlety', 'subtonic', 'subtotal', 'subtract', 'suburban', 'suburbia', 'subverts', 'succeeds', 'succinct', 'succored', 'succumbs', 'suchlike', 'suckered', 'suckling', 'suctions', 'sudanese', 'suddenly', 'sudsiest', 'suffered', 'sufferer', 'sufficed', 'suffices', 'suffixed', 'suffixes', 'suffrage', 'suffused', 'suffuses', 'sugaring', 'suggests', 'suicides', 'suicidal', 'suitably', 'suitings', 'suitable', 'suitcase', 'sukiyaki', 'sulfates', 'sulfides', 'sulfites', 'sulfuric', 'sulkiest', 'sullying', 'sullener', 'sullenly', 'sultanas', 'sultrier', 'summered', 'summoned', 'sunbathe', 'sunbeams', 'sunburns', 'sunburst', 'sundered', 'sundials', 'sundowns', 'sundries', 'sunlight', 'sunniest', 'sunrises', 'sunshade', 'sunshine', 'sunshiny', 'sunspots', 'superbly', 'superego', 'superior', 'supermen', 'superman', 'supernal', 'supinely', 'supplies', 'supplest', 'supplant', 'supplied', 'supplier', 'supports', 'supposes', 'supposed', 'suppress', 'surcease', 'surcoats', 'sureness', 'surefire', 'sureties', 'surfaced', 'surfaces', 'surfeits', 'surgeons', 'surgical', 'surliest', 'surmised', 'surmises', 'surmount', 'surnamed', 'surnames', 'surplice', 'surprint', 'surprise', 'surround', 'surtaxes', 'surveyed', 'surveyor', 'survived', 'survives', 'survivor', 'survival', 'suspects', 'suspends', 'suspense', 'sustains', 'suturing', 'suzerain', 'suzettes', 'sveltest', 'swabbing', 'swaddles', 'swaddled', 'swaggers', 'swallows', 'swamping', 'swampier', 'swankier', 'swankest', 'swapping', 'swappers', 'swarming', 'swashing', 'swastika', 'swatches', 'swathing', 'swatting', 'swatters', 'swayback', 'swearing', 'sweating', 'sweaters', 'sweatier', 'sweepers', 'sweeping', 'sweetest', 'sweetens', 'swelling', 'swelters', 'swerving', 'swiftest', 'swigging', 'swilling', 'swimming', 'swimmers', 'swimsuit', 'swindles', 'swindled', 'swindler', 'swinging', 'swingers', 'swirling', 'swishing', 'switched', 'switches', 'swiveled', 'swizzles', 'swizzled', 'swooning', 'swooping', 'sycamore', 'syllabic', 'syllable', 'syllabub', 'syllabus', 'symboled', 'symbolic', 'symmetry', 'sympathy', 'symphony', 'symposia', 'symptoms', 'synapses', 'syncline', 'syncopes', 'syndrome', 'synonyms', 'synonymy', 'synopses', 'synopsis', 'synoptic', 'syntaxes', 'syphilis', 'syringes', 'systemic', 'systoles', 'systolic', 'tableaux', 'tablehop', 'tabletop', 'tabloids', 'tabooing', 'tabulate', 'taciturn', 'tackiest', 'tackling', 'tacklers', 'taconite', 'tactical', 'tactless', 'tadpoles', 'taffetas', 'tahitian', 'tailback', 'tailgate', 'tailored', 'tailpipe', 'tailrace', 'tailspin', 'tailwind', 'tainting', 'takedown', 'takeoffs', 'takeover', 'talented', 'talisman', 'talkiest', 'tallness', 'tallyhos', 'tallying', 'talmudic', 'tamarack', 'tamarind', 'tamarisk', 'tambours', 'tameness', 'tampered', 'tanagers', 'tanbarks', 'tangiest', 'tangelos', 'tangents', 'tangibly', 'tangible', 'tangling', 'tangoing', 'tankards', 'tantalum', 'tantalus', 'tantrums', 'tanzania', 'tapering', 'tapestry', 'tapeworm', 'tapiocas', 'taprooms', 'taproots', 'tapsters', 'tardiest', 'targeted', 'tarlatan', 'tarpaper', 'tarrying', 'tarragon', 'tartness', 'tartaric', 'tasmania', 'tasseled', 'tastiest', 'tasteful', 'tattered', 'tattling', 'tattlers', 'tattooed', 'taunting', 'tautness', 'tawdrier', 'tawniest', 'taxation', 'taxicabs', 'taxonomy', 'taxpayer', 'teacarts', 'teaching', 'teachers', 'teahouse', 'teammate', 'teamster', 'teamwork', 'teardrop', 'tearooms', 'teaspoon', 'tectonic', 'teeniest', 'teetered', 'teething', 'tegument', 'tektites', 'telecast', 'telegram', 'teleplay', 'telethon', 'teletype', 'televise', 'telexing', 'telltale', 'temerity', 'tempered', 'temperas', 'tempests', 'template', 'temporal', 'tempters', 'tempting', 'tempuras', 'tenacity', 'tenanted', 'tendered', 'tendency', 'tenderly', 'tendrils', 'tenement', 'tensions', 'tentacle', 'tepidity', 'tequilas', 'terabits', 'teriyaki', 'terminal', 'terminus', 'termites', 'terraced', 'terraces', 'terrains', 'terrapin', 'terrazzo', 'terribly', 'terrible', 'terriers', 'terrific', 'tertians', 'tertiary', 'testiest', 'testable', 'testings', 'testator', 'testicle', 'tethered', 'tetrarch', 'teutonic', 'textbook', 'textiles', 'textured', 'textures', 'textural', 'thailand', 'thalamic', 'thalamus', 'thallium', 'thanking', 'thankful', 'thatched', 'thatches', 'theaters', 'theatres', 'theistic', 'thematic', 'theocrat', 'theology', 'theories', 'theorist', 'theorems', 'theorize', 'therefor', 'thermion', 'thermals', 'thespian', 'thiamine', 'thickest', 'thickens', 'thickets', 'thickset', 'thieving', 'thievery', 'thievish', 'thimbles', 'thinness', 'thinking', 'thinkers', 'thinning', 'thinners', 'thinnest', 'thirsted', 'thirties', 'thirteen', 'thistles', 'tholepin', 'thoracic', 'thoraxes', 'thornier', 'thorough', 'thoughts', 'thousand', 'thrashed', 'thrasher', 'thrashes', 'threaded', 'threaten', 'threnody', 'threshed', 'thresher', 'threshes', 'thrilled', 'thriller', 'thriving', 'throbbed', 'thrombin', 'thrombus', 'throning', 'thronged', 'throttle', 'throwing', 'thrummed', 'thrushes', 'thruways', 'thudding', 'thumbing', 'thumbnut', 'thumping', 'thunders', 'thurible', 'thursday', 'thwacked', 'thwarted', 'thymuses', 'thyroids', 'thyroxin', 'tibetans', 'tickings', 'ticketed', 'tickling', 'ticklers', 'ticklish', 'tideland', 'tidemark', 'tidiness', 'tiebacks', 'tightest', 'tightens', 'tightwad', 'tillable', 'tillages', 'timbered', 'timbales', 'timecard', 'timelier', 'timeless', 'timeline', 'timeworn', 'timidity', 'timidest', 'timorous', 'tincture', 'tinfoils', 'tingeing', 'tingling', 'tinhorns', 'tinkered', 'tinkling', 'tinniest', 'tinseled', 'tinsmith', 'tintyped', 'tintypes', 'tippling', 'tipplers', 'tipsiest', 'tipsters', 'tireless', 'tiresome', 'titanium', 'titlarks', 'titmouse', 'tittered', 'toadying', 'toadfish', 'toasting', 'toasters', 'tobaccos', 'toboggan', 'toccatas', 'toddling', 'toddlers', 'toeholds', 'toenails', 'together', 'toggling', 'toiletry', 'toilette', 'tokening', 'tokenism', 'tolerant', 'tolerate', 'tollgate', 'toluenes', 'tomahawk', 'tomalley', 'tomatoes', 'tomfools', 'tommyrot', 'tomorrow', 'tonality', 'tonguing', 'tonights', 'tonnages', 'tonneaus', 'tonsured', 'tonsures', 'tontines', 'toolings', 'toothily', 'toothier', 'topcoats', 'topknots', 'topmasts', 'topnotch', 'topology', 'toppings', 'toppling', 'topsails', 'topsider', 'topsides', 'topsoils', 'torching', 'toreador', 'torments', 'torrents', 'torsions', 'tortilla', 'tortoise', 'tortuous', 'tortured', 'torturer', 'tortures', 'totaling', 'totality', 'tottered', 'touching', 'touchier', 'touchups', 'toughing', 'toughest', 'toughens', 'tourists', 'tourisms', 'tourneys', 'tousling', 'toweling', 'towering', 'towheads', 'towlines', 'township', 'townsmen', 'townsman', 'towpaths', 'towropes', 'toxemias', 'toxicity', 'toxicant', 'tracings', 'tracheae', 'tracheal', 'trachoma', 'tracking', 'trackage', 'traction', 'tractors', 'tradeoff', 'traduced', 'traducer', 'traduces', 'traffics', 'trailing', 'trailers', 'training', 'trainers', 'trainees', 'trainmen', 'trainman', 'traipsed', 'traipses', 'traitors', 'trammels', 'tramping', 'tramples', 'trampled', 'tramways', 'tranquil', 'transact', 'transept', 'transfer', 'transfix', 'transits', 'transmit', 'transoms', 'trapezes', 'trapping', 'trappers', 'trashing', 'trashily', 'trashier', 'traumata', 'travails', 'traveled', 'traveler', 'traverse', 'travesty', 'trawling', 'trawlers', 'treacles', 'treading', 'treadles', 'treadled', 'treasons', 'treasure', 'treasury', 'treating', 'treaties', 'treatise', 'trebling', 'treeless', 'treetops', 'trefoils', 'trekking', 'trembles', 'trembled', 'tremolos', 'trenched', 'trenches', 'trending', 'trendier', 'trepangs', 'trephine', 'trespass', 'trestles', 'triangle', 'triassic', 'tribunes', 'tribunal', 'tributes', 'trichina', 'tricking', 'trickily', 'trickier', 'trickles', 'trickery', 'trickled', 'tricolor', 'tricorns', 'trycycle', 'tridents', 'trifling', 'trifocal', 'trigging', 'triggers', 'trilling', 'trillion', 'trillium', 'trimming', 'trimmers', 'trimmest', 'trinidad', 'trinkets', 'trioxide', 'tripling', 'triplets', 'tripping', 'triptych', 'triremes', 'trisects', 'triumphs', 'triumvir', 'trochees', 'trolling', 'trolleys', 'trollops', 'trombone', 'tromping', 'trooping', 'troopers', 'tropisms', 'trophies', 'tropical', 'trotting', 'trotters', 'trounces', 'troubles', 'troubled', 'trounced', 'trouping', 'troupers', 'trousers', 'troweled', 'trucking', 'truckers', 'truckles', 'truckage', 'truckled', 'truckmen', 'truckman', 'trudging', 'trueblue', 'truelove', 'truffles', 'trumpery', 'trumpets', 'truncate', 'trundles', 'trundled', 'trussing', 'trustier', 'trustees', 'trusting', 'truthful', 'tsunamis', 'tuataras', 'tubbiest', 'tubeless', 'tuberous', 'tubercle', 'tuberose', 'tuckered', 'tuesdays', 'tugboats', 'tuitions', 'tumbling', 'tumblers', 'tumbrels', 'tumefied', 'tumefies', 'tumerics', 'tumidity', 'tuneless', 'tungsten', 'tunisian', 'tunneled', 'turbines', 'turbojet', 'turmeric', 'turmoils', 'turnings', 'turncoat', 'turnkeys', 'turnoffs', 'turnouts', 'turnover', 'turnpike', 'turreted', 'tussling', 'tussocks', 'tutelage', 'tutelary', 'tutoring', 'tutorial', 'twaddles', 'twaddled', 'twanging', 'tweaking', 'tweedier', 'tweeters', 'tweezers', 'twelfths', 'twenties', 'twiddles', 'twiddled', 'twilight', 'twilling', 'twinging', 'twinight', 'twinkles', 'twinkled', 'twirling', 'twisting', 'twisters', 'twitched', 'twitches', 'twitting', 'twitters', 'twosomes', 'tympanic', 'tympanum', 'typecast', 'typeface', 'typesets', 'typhoids', 'typhoons', 'typified', 'typifies', 'typology', 'tyrosine', 'ubiquity', 'ugandans', 'ugliness', 'ukuleles', 'ulcerous', 'ulcerate', 'ulterior', 'ultimate', 'ululated', 'ululates', 'umbering', 'umbilici', 'umbraged', 'umbrages', 'umbrella', 'umpiring', 'umpteens', 'unabated', 'unafraid', 'unawares', 'unbarred', 'unbeaten', 'unbelief', 'unbiased', 'unbidden', 'unbolted', 'unbosoms', 'unbroken', 'unbuckle', 'unburden', 'unburied', 'unbutton', 'uncapped', 'uncaring', 'uncenter', 'unchains', 'unchaste', 'unciform', 'unclasps', 'unclench', 'uncloaks', 'unclosed', 'uncloses', 'unclothe', 'uncoiled', 'uncommon', 'uncooked', 'uncorked', 'uncouple', 'uncovers', 'unctions', 'unctuous', 'uncurled', 'underact', 'underage', 'underarm', 'underbid', 'undercut', 'underdog', 'underfed', 'underlay', 'underlie', 'underpay', 'underpin', 'undersea', 'undertow', 'undimmed', 'undoings', 'undulant', 'undulate', 'unearned', 'unearths', 'uneasily', 'unending', 'unequals', 'unerring', 'unevenly', 'unfairly', 'unfasten', 'unfenced', 'unfilled', 'unfitted', 'unflawed', 'unfolded', 'unforced', 'unformed', 'unfrocks', 'unfrozen', 'unfurled', 'ungainly', 'unglazed', 'ungluing', 'unguents', 'ungulate', 'unhanded', 'unharmed', 'unhealed', 'unheated', 'unheeded', 'unhinged', 'unhinges', 'unhooked', 'unhorsed', 'unhorses', 'unicorns', 'unicycle', 'unifying', 'unifiers', 'uniforms', 'unionist', 'unionism', 'unionize', 'uniquely', 'univalve', 'universe', 'univocal', 'unjustly', 'unkindly', 'unknowns', 'unlacing', 'unlawful', 'unleaded', 'unlearns', 'unlikely', 'unlimber', 'unlisted', 'unloaded', 'unlocked', 'unloosed', 'unlooses', 'unloosen', 'unlovely', 'unmaking', 'unmanned', 'unmarked', 'unmarred', 'unmasked', 'unmoving', 'unneeded', 'unnerved', 'unnerves', 'unopened', 'unpacked', 'unpinned', 'unplowed', 'unproved', 'unravels', 'unreason', 'unreeled', 'unripest', 'unrolled', 'unsaddle', 'unsalted', 'unsavory', 'unscrews', 'unsealed', 'unseated', 'unseeing', 'unseemly', 'unsettle', 'unsexing', 'unshaken', 'unshaped', 'unsigned', 'unsnarls', 'unsoiled', 'unsolved', 'unsorted', 'unsought', 'unspoken', 'unstably', 'unstable', 'unsteady', 'unstring', 'unstrung', 'unsubtle', 'unsuited', 'untangle', 'untapped', 'untaught', 'untested', 'untidily', 'untimely', 'untiring', 'untitled', 'untoward', 'untruths', 'untwined', 'untwines', 'untwists', 'unusable', 'unveiled', 'unversed', 'unvoiced', 'unwanted', 'unwarily', 'unwashed', 'unwieldy', 'unwisely', 'unwonted', 'unworthy', 'unyoking', 'unzipped', 'upbraids', 'upcoming', 'updating', 'updrafts', 'upending', 'upgraded', 'upgrades', 'upheaved', 'upheaves', 'upheaval', 'uplifted', 'uppercut', 'upraised', 'upraises', 'uprights', 'uprising', 'uprooted', 'upstaged', 'upstages', 'upstairs', 'upstarts', 'upstates', 'upstream', 'upstroke', 'upsurged', 'upsurges', 'upswings', 'upsweeps', 'upturned', 'uraniums', 'urbanity', 'urbanite', 'urbanize', 'urethral', 'urethras', 'urgently', 'urinated', 'urinates', 'usefully', 'ushering', 'usufruct', 'usurious', 'usurping', 'utensils', 'uteruses', 'utilized', 'utilizes', 'utopians', 'uttering', 'uxorious', 'vacantly', 'vacating', 'vacation', 'vaccines', 'vacuoles', 'vacuumed', 'vagabond', 'vagaries', 'vagrancy', 'vagrants', 'valances', 'valanced', 'valences', 'valerian', 'valeting', 'valhalla', 'valiants', 'validity', 'validate', 'valkyrie', 'valorous', 'valuable', 'valuator', 'valvular', 'vamoosed', 'vamooses', 'vampires', 'vanadium', 'vandykes', 'vanguard', 'vanillas', 'vanillin', 'vanished', 'vanishes', 'vanities', 'vanquish', 'vantages', 'vapidity', 'vaporing', 'vaporous', 'vaporize', 'vaqueros', 'variably', 'variance', 'variable', 'variants', 'varicose', 'varmints', 'vascular', 'vaseline', 'vastness', 'vaulting', 'vaunting', 'vectored', 'vegetate', 'vehement', 'vehicles', 'veilings', 'veinings', 'velleity', 'velocity', 'venality', 'venation', 'vendetta', 'vendible', 'veneered', 'venerate', 'venereal', 'venetian', 'vengeful', 'venisons', 'venomous', 'ventrals', 'ventured', 'ventures', 'veracity', 'verandah', 'verbally', 'verbatim', 'verbenas', 'verbiage', 'verboten', 'verdancy', 'verdicts', 'verdures', 'verified', 'verifier', 'verifies', 'verities', 'vermeils', 'vermouth', 'verniers', 'veronica', 'versions', 'versicle', 'vertebra', 'vertexes', 'vertical', 'vertices', 'vesicles', 'vesicant', 'vesicate', 'vestment', 'vestiges', 'vestries', 'vestured', 'vestures', 'veterans', 'vexation', 'viaducts', 'viaticum', 'vibrancy', 'vibrated', 'vibrates', 'vibratos', 'vibrator', 'viburnum', 'vicarage', 'viceroys', 'vicinity', 'vicinage', 'victoria', 'victrola', 'victuals', 'viennese', 'vietcong', 'vietminh', 'vietnams', 'vigilant', 'vignette', 'vigorous', 'vilified', 'vilifies', 'villager', 'villages', 'villains', 'villainy', 'villeins', 'vincible', 'vinculum', 'vinegars', 'vinegary', 'vineyard', 'vintaged', 'vintages', 'vintners', 'violists', 'violably', 'violable', 'violated', 'violates', 'violator', 'violence', 'viperous', 'viragoes', 'virginal', 'virginia', 'virgules', 'virility', 'virology', 'virtuoso', 'virtuous', 'virulent', 'visceral', 'viscoses', 'viscount', 'viselike', 'visigoth', 'visioned', 'visiting', 'visitors', 'visitant', 'visoring', 'visually', 'vitality', 'vitalize', 'vitamins', 'vitiated', 'vitiates', 'vitiator', 'vitreous', 'vitriols', 'vivacity', 'vividest', 'vivified', 'vivifies', 'vivisect', 'vocalist', 'vocalics', 'vocalize', 'vocation', 'vocative', 'voidable', 'volatile', 'volcanic', 'volition', 'volleyed', 'voltages', 'vomiting', 'voodooed', 'voracity', 'vortexes', 'votaries', 'vouching', 'vouchers', 'voyaging', 'voyagers', 'voyageur', 'vulgates', 'vultures', 'wackiest', 'waddings', 'waddling', 'waffling', 'wagering', 'waggling', 'wainscot', 'waitress', 'wakening', 'walkaway', 'walkouts', 'walleyed', 'walleyes', 'walloons', 'walloped', 'wallowed', 'walruses', 'waltzing', 'wandered', 'wanderer', 'wangling', 'wantoned', 'wantonly', 'warbling', 'warblers', 'wardrobe', 'wardroom', 'wardship', 'warfares', 'warheads', 'wariness', 'warlocks', 'warlords', 'warnings', 'warpaths', 'warplane', 'warrants', 'warranty', 'warriors', 'warships', 'wartimes', 'washings', 'washable', 'washbowl', 'washdays', 'washouts', 'washroom', 'washtubs', 'wassails', 'wastings', 'wastages', 'wasteful', 'wastrels', 'watching', 'watchers', 'watchdog', 'watchful', 'watchmen', 'watchman', 'watering', 'waterier', 'waterloo', 'waterway', 'wattages', 'wattling', 'waveband', 'wavering', 'waviness', 'waxiness', 'waxwings', 'waxworks', 'waybills', 'wayfarer', 'waysides', 'weakened', 'weakfish', 'weakling', 'weaklier', 'weakness', 'weaponed', 'weaponry', 'weariest', 'wearying', 'wearable', 'weaseled', 'weathers', 'weavings', 'webbings', 'weddings', 'wedlocks', 'weediest', 'weekdays', 'weekends', 'weeklies', 'weepiest', 'weighing', 'weighted', 'weirdest', 'welcomed', 'welcomes', 'welfares', 'welshing', 'weltings', 'weltered', 'wenching', 'werewolf', 'westerly', 'westerns', 'westward', 'wetbacks', 'wetlands', 'whacking', 'whamming', 'whammies', 'wharfage', 'whatever', 'whatnots', 'wheedles', 'wheedled', 'wheeling', 'wheezing', 'wheezily', 'wheezier', 'whelming', 'whelping', 'whenever', 'wherever', 'wherries', 'whethers', 'whetting', 'whiffing', 'whimpers', 'whimsies', 'whiniest', 'whinnied', 'whinnies', 'whipcord', 'whiplash', 'whipping', 'whippers', 'whippets', 'whipsaws', 'whirling', 'whirring', 'whisking', 'whiskers', 'whiskeys', 'whispers', 'whistles', 'whistled', 'whistler', 'whitecap', 'whitened', 'whitener', 'whiteout', 'whitlows', 'whittles', 'whittled', 'whizzing', 'whodunit', 'whomever', 'whooping', 'whooshed', 'whooshes', 'whopping', 'whoppers', 'wickeder', 'wickedly', 'wickiups', 'widening', 'widowing', 'widowers', 'wielding', 'wiggling', 'wildness', 'wildcats', 'wildfire', 'wildfowl', 'wildlife', 'wilfully', 'wiliness', 'wimpling', 'wimching', 'windiest', 'windings', 'windbags', 'windfall', 'windlass', 'windless', 'windmill', 'windowed', 'windpipe', 'windrows', 'windsock', 'windward', 'wineries', 'wineskin', 'wingding', 'wingspan', 'winnings', 'winnowed', 'wintered', 'wintrier', 'wireless', 'wiretaps', 'wiriness', 'wiseacre', 'wishbone', 'wispiest', 'wisteria', 'witching', 'witchery', 'withered', 'withdraw', 'withdrew', 'withheld', 'withhold', 'wittiest', 'wizardry', 'wizening', 'wobbling', 'wobblier', 'wobblies', 'woefully', 'womanish', 'womanize', 'wondered', 'wondrous', 'woodiest', 'woodbine', 'woodcock', 'woodcuts', 'woodland', 'woodpile', 'woodshed', 'woodsmen', 'woodsman', 'woodwind', 'woodwork', 'woollier', 'woollies', 'wooziest', 'wordiest', 'wordings', 'wordages', 'wordbook', 'wordless', 'workable', 'workings', 'workaday', 'workbook', 'workdays', 'workhand', 'workload', 'workouts', 'workroom', 'workshop', 'workweek', 'wormhole', 'wormwood', 'worrying', 'worriers', 'worsened', 'worships', 'worsting', 'worsteds', 'worthier', 'worthies', 'wounding', 'wracking', 'wrangles', 'wrangled', 'wrangler', 'wrapping', 'wrappers', 'wrathful', 'wreaking', 'wreathed', 'wreathes', 'wrecking', 'wreckers', 'wreckage', 'wrenched', 'wrenches', 'wresting', 'wrestles', 'wrestled', 'wrestler', 'wretches', 'wretched', 'wriggles', 'wriggled', 'wringing', 'wringers', 'wrinkles', 'wrinkled', 'writings', 'writhing', 'wronging', 'wrongful', 'xeroxing', 'yachting', 'yardages', 'yardarms', 'yarmulke', 'yearbook', 'yearling', 'yearlong', 'yearning', 'yeastier', 'yellowed', 'yeomanry', 'yeshivas', 'yielding', 'yodeling', 'youngest', 'youngish', 'yourself', 'youthful', 'ytterbic', 'yttriums', 'yugoslav', 'yuletide', 'yummiest', 'zambians', 'zeppelin', 'ziggurat', 'zillions', 'zimbabwe', 'zionists', 'zippiest', 'zippered', 'zodiacal', 'zoophyte', 'zoospore', 'zucchini', 'zwieback', 'zymogens', 'zymology', 'zyzzyvas', 'aardvarks', 'abandoned', 'abasement', 'abatement', 'abattoirs', 'abdicated', 'abdicates', 'abdicator', 'abdominal', 'abducting', 'abduction', 'abductors', 'aberrance', 'abeyances', 'abhorring', 'abhorrers', 'abhorrent', 'abidances', 'abidingly', 'abilities', 'abjection', 'ablations', 'ablatives', 'ablutions', 'abnegated', 'abnegates', 'abnegator', 'abolished', 'abolishes', 'abolition', 'abominate', 'aborigine', 'abortions', 'abounding', 'abrasions', 'abrasives', 'abridging', 'abridgers', 'abrogated', 'abrogates', 'abscessed', 'abscesses', 'abscising', 'abscissas', 'absconded', 'absenting', 'absentees', 'absinthes', 'absolutes', 'absolving', 'absolvers', 'absorbing', 'absorbers', 'absorbent', 'abstained', 'abstinent', 'abstracts', 'absurdity', 'abundance', 'abusively', 'abutments', 'abysmally', 'academies', 'academics', 'accedence', 'accenting', 'accentual', 'accepting', 'acceptors', 'accessing', 'accession', 'accessory', 'accidence', 'accidents', 'acclaimed', 'acclimate', 'acclivity', 'accolades', 'accompany', 'accomplis', 'according', 'accordion', 'accosting', 'accounted', 'accouters', 'accredits', 'accretion', 'accruable', 'accustoms', 'acerbated', 'acerbates', 'acetylene', 'achieving', 'achievers', 'aciculate', 'acidities', 'acidified', 'acidifier', 'acidifies', 'acidulate', 'acidulous', 'acoustics', 'acquaints', 'acquiesce', 'acquiring', 'acquirers', 'acquitted', 'acquittor', 'acquittal', 'acrobatic', 'acronymic', 'acropolis', 'acrostics', 'actinisms', 'actinides', 'actiniums', 'activists', 'activisms', 'activated', 'activates', 'activator', 'actresses', 'actuality', 'actualize', 'actuaries', 'actuarial', 'actuating', 'actuation', 'actuators', 'acuteness', 'adamantly', 'adaptable', 'addicting', 'addiction', 'addictive', 'additions', 'additives', 'addressed', 'addresser', 'addresses', 'addressee', 'adducting', 'adduction', 'adductive', 'adductors', 'adenoidal', 'adenosine', 'adeptness', 'adherence', 'adherents', 'adhesions', 'adhesives', 'adiabatic', 'adiposity', 'adjacency', 'adjective', 'adjoining', 'adjourned', 'adjudging', 'adjusting', 'adjusters', 'adjutants', 'admirably', 'admirable', 'admiralty', 'admission', 'admissive', 'admitting', 'admixture', 'adoptions', 'adoration', 'adoringly', 'adornment', 'adrenalin', 'adsorbing', 'adsorbent', 'adulating', 'adulation', 'adulators', 'adulatory', 'adulterer', 'adulthood', 'adumbrate', 'advancing', 'advancers', 'advantage', 'adventist', 'adventure', 'adverbial', 'adversity', 'adversary', 'adversely', 'adverting', 'advertise', 'advisably', 'advisable', 'advisedly', 'advocated', 'advocates', 'advocator', 'aerations', 'aerialist', 'aeronauts', 'aeropause', 'aerospace', 'aerostats', 'aesthetes', 'aesthetic', 'aestivate', 'affection', 'affective', 'affecting', 'affiances', 'affianced', 'affidavit', 'affiliate', 'affirming', 'affirmant', 'afflation', 'afflicted', 'affluence', 'affluents', 'affording', 'affronted', 'aforesaid', 'afrikaans', 'aftercare', 'afterdeck', 'afterglow', 'afterlife', 'aftermath', 'afternoon', 'afterward', 'ageratums', 'aggravate', 'aggregate', 'aggressed', 'aggresses', 'aggressor', 'aggrieved', 'aggrieves', 'agilities', 'agitating', 'agitation', 'agitators', 'agnostics', 'agonizing', 'agrarians', 'agreeably', 'agreement', 'agreeable', 'agronomic', 'aigrettes', 'ailanthus', 'aimlessly', 'airdromes', 'airedales', 'airfields', 'airlifted', 'airliners', 'airmailed', 'airplanes', 'airspaces', 'airstrips', 'airworthy', 'alabaster', 'alarmists', 'albacores', 'albatross', 'alchemies', 'alchemist', 'alcoholic', 'aldehydes', 'alehouses', 'alertness', 'algebraic', 'algerians', 'algorithm', 'alienists', 'alienable', 'alienated', 'alienates', 'alienator', 'alighting', 'alignment', 'alikeness', 'alimonies', 'aliphatic', 'alkalized', 'alkalizes', 'alkaloids', 'alkaloses', 'alkalosis', 'allantois', 'allayment', 'allegedly', 'allegoric', 'alleluias', 'allemande', 'allergies', 'allergist', 'allergens', 'alleviate', 'alleyways', 'alliances', 'alligator', 'allocable', 'allocated', 'allocates', 'allomorph', 'allopathy', 'allophone', 'allotment', 'allotrope', 'allotropy', 'allotting', 'allowably', 'allowable', 'allowance', 'allspices', 'allusions', 'alluviums', 'almsgiver', 'aloneness', 'alongside', 'aloofness', 'alpenhorn', 'alphabets', 'alterable', 'altercate', 'alternate', 'altimeter', 'altitudes', 'altruists', 'altruisms', 'aluminous', 'aluminize', 'aluminums', 'alveolars', 'amaranths', 'amaryllis', 'amassment', 'amazement', 'amazingly', 'ambergris', 'ambiances', 'ambiences', 'ambiguity', 'ambiguous', 'ambitions', 'ambitious', 'ambrosial', 'ambrosias', 'ambulance', 'ambulated', 'ambulates', 'ambuscade', 'ambushing', 'amenities', 'amendment', 'americans', 'americana', 'americium', 'amethysts', 'amidships', 'ammoniacs', 'ammoniate', 'ammonites', 'amnesiacs', 'amnesties', 'amorality', 'amorously', 'amorphous', 'amortized', 'amortizes', 'amounting', 'amperages', 'ampersand', 'amphibian', 'amphibole', 'amplified', 'amplifier', 'amplifies', 'amplitude', 'amputated', 'amputates', 'amputator', 'amsterdam', 'amusement', 'amusingly', 'amylopsin', 'anabolism', 'anacondas', 'anaerobes', 'anaerobic', 'analgesic', 'analgesia', 'analogies', 'analogous', 'analogize', 'analogues', 'analyzing', 'analyzers', 'ananiases', 'anaphases', 'anaphoric', 'anarchies', 'anarchism', 'anarchist', 'anathemas', 'anatomies', 'anatomist', 'anatomize', 'ancestors', 'ancestral', 'anchoring', 'anchorage', 'anchorite', 'anchormen', 'anchorman', 'anchovies', 'anciently', 'ancillary', 'andantino', 'androgens', 'androgyny', 'anecdotes', 'anecdotal', 'aneurysms', 'angelfish', 'angelicas', 'angleworm', 'anglicans', 'anglicize', 'angostura', 'angstroms', 'anguished', 'anguishes', 'anhydrous', 'anhydride', 'anhydrite', 'animation', 'animalism', 'animating', 'animators', 'animistic', 'animosity', 'anisettes', 'anklebone', 'annalists', 'annapolis', 'annealing', 'annotated', 'annotates', 'annotator', 'announced', 'announcer', 'announces', 'annoyance', 'annuities', 'annuitant', 'annulment', 'annulling', 'annuluses', 'anodizing', 'anointing', 'anomalies', 'anomalous', 'anonymity', 'anonymous', 'anopheles', 'anoretics', 'anorexics', 'answering', 'antarctic', 'anteaters', 'antedated', 'antedates', 'antelopes', 'anterooms', 'anthology', 'anthozoan', 'anthraces', 'anticline', 'antidotes', 'antidotal', 'antigenic', 'antiknock', 'antipasto', 'antipathy', 'antiphons', 'antiphony', 'antipodes', 'antipodal', 'antiquing', 'antiquity', 'antiquary', 'antiquate', 'antitoxic', 'antitoxin', 'antitrust', 'antivenin', 'anxieties', 'anxiously', 'apartment', 'apartheid', 'apathetic', 'aperitifs', 'apertures', 'aphasiacs', 'aphorisms', 'aphrodite', 'apiarists', 'apocrypha', 'apologist', 'apologies', 'apologias', 'apologize', 'apostates', 'apostolic', 'apothegms', 'appalling', 'appaloosa', 'appanages', 'apparatus', 'appareled', 'appealing', 'appearing', 'appeasing', 'appeasers', 'appellant', 'appellate', 'appellees', 'appending', 'appendage', 'appertain', 'appetites', 'appetizer', 'applauded', 'applejack', 'appliance', 'applicant', 'appliques', 'appliqued', 'appointed', 'appointee', 'apportion', 'appraised', 'appraiser', 'appraises', 'appraisal', 'apprehend', 'apprising', 'approvers', 'approvals', 'approving', 'aptitudes', 'aquanauts', 'aquaplane', 'aquariums', 'aquatints', 'aqueducts', 'arabesque', 'arachnids', 'aragonite', 'arbitrary', 'arbitrate', 'arboretum', 'arbutuses', 'archaisms', 'archangel', 'archducal', 'archdukes', 'archenemy', 'archetype', 'archfiend', 'architect', 'archiving', 'archivist', 'ardencies', 'arduously', 'argentine', 'argentina', 'argentite', 'arguments', 'aridities', 'armaments', 'armadillo', 'armatures', 'armchairs', 'armenians', 'armistice', 'armorials', 'aromatics', 'arpeggios', 'arraigned', 'arranging', 'arrangers', 'arresting', 'arrogance', 'arrogated', 'arrogates', 'arrogator', 'arrowhead', 'arrowroot', 'arsenates', 'arsonists', 'arterials', 'arteriole', 'arthritic', 'arthritis', 'arthropod', 'arthurian', 'artichoke', 'articling', 'articular', 'artifacts', 'artifices', 'artillery', 'artlessly', 'asafetida', 'ascending', 'ascendant', 'ascension', 'ascertain', 'ascribing', 'asininity', 'asparagus', 'aspersing', 'aspersion', 'asphodels', 'aspirants', 'aspirated', 'aspirates', 'aspirator', 'assailing', 'assailant', 'assassins', 'assaulted', 'assembles', 'assembled', 'assembler', 'assenting', 'asserting', 'assertion', 'assertive', 'assessing', 'assessors', 'assiduity', 'assiduous', 'assigning', 'assignees', 'assisting', 'assistant', 'associate', 'assonance', 'assorting', 'assuaging', 'assurance', 'assuredly', 'astatines', 'asterisms', 'asterisks', 'asteroids', 'asthenias', 'asthmatic', 'astounded', 'astraddle', 'astrakhan', 'astrodome', 'astrolabe', 'astrology', 'astronaut', 'astronomy', 'astroturf', 'asymmetry', 'asymptote', 'ataractic', 'atavistic', 'atheistic', 'athenaeum', 'athenians', 'athletics', 'atomizing', 'atomizers', 'atonality', 'atonement', 'atrocious', 'atrophies', 'atrophied', 'atropines', 'attaching', 'attacking', 'attackers', 'attaining', 'attainder', 'attainted', 'attempted', 'attending', 'attendant', 'attention', 'attentive', 'attenuate', 'attesting', 'attitudes', 'attorneys', 'attracted', 'attribute', 'attrition', 'auctioned', 'auctorial', 'audacious', 'audiences', 'auditions', 'augmented', 'auricular', 'austerity', 'austerely', 'australis', 'australia', 'austrians', 'authentic', 'authoring', 'authority', 'authorial', 'authorize', 'autobahns', 'autoclave', 'autocracy', 'autocrats', 'autograph', 'autoharps', 'automated', 'automates', 'automatic', 'automaton', 'autonomic', 'autopsies', 'auxiliary', 'availably', 'available', 'avalanche', 'averaging', 'aversions', 'aviarists', 'avidities', 'avocation', 'avoidably', 'avoidable', 'avoidance', 'avouching', 'avuncular', 'awakening', 'awareness', 'awfulness', 'awkwarder', 'awkwardly', 'axiomatic', 'axletrees', 'babushkas', 'baccarats', 'bacchanal', 'bachelors', 'bacillary', 'backaches', 'backbiter', 'backbites', 'backboard', 'backbones', 'backdrops', 'backfield', 'backfired', 'backfires', 'backhands', 'backorder', 'backpacks', 'backrests', 'backsides', 'backslash', 'backslide', 'backspace', 'backspins', 'backstage', 'backstops', 'backtrack', 'backwards', 'backwater', 'backwoods', 'backyards', 'bacterial', 'bacterium', 'badgering', 'badinages', 'badminton', 'badmouths', 'bagatelle', 'bagginess', 'bagpipers', 'baguettes', 'bahamians', 'bailiwick', 'balalaika', 'balancing', 'balconies', 'baldachin', 'baldpates', 'balkiness', 'balladeer', 'ballerina', 'ballistic', 'ballooned', 'balloting', 'ballrooms', 'ballyhoos', 'balminess', 'baltimore', 'balusters', 'bamboozle', 'bandaging', 'bandannas', 'bandboxes', 'banderole', 'bandicoot', 'bandoleer', 'bandstand', 'bandwagon', 'banishing', 'banisters', 'banjoists', 'bankbooks', 'bankrolls', 'bankrupts', 'bannering', 'banqueted', 'banquette', 'bantering', 'baptismal', 'baptizing', 'barbering', 'barbarism', 'barbarity', 'barbarous', 'barbarian', 'barbecued', 'barbecues', 'barbicans', 'barbitals', 'barcarole', 'barefaced', 'bargained', 'baritones', 'barkeeper', 'barnacles', 'barnstorm', 'barnyards', 'barograph', 'barometer', 'barometry', 'barouches', 'barracuda', 'barraging', 'barreling', 'barrettes', 'barricade', 'barrister', 'bartender', 'basements', 'baseballs', 'baseboard', 'bashfully', 'basically', 'basilicas', 'basilisks', 'bassinets', 'basswoods', 'bastinado', 'bathhouse', 'batholith', 'bathrobes', 'bathrooms', 'battalion', 'battening', 'battering', 'batteries', 'bawdiness', 'bayoneted', 'beachhead', 'beanpoles', 'beanstalk', 'beardless', 'bearnaise', 'bearskins', 'beastlier', 'beatified', 'beatifies', 'beatitude', 'beauteous', 'beautiful', 'becalming', 'beckoning', 'beclouded', 'becomings', 'bedaubing', 'bedazzled', 'bedazzles', 'bedecking', 'bedeviled', 'bedfellow', 'bedizened', 'bedraggle', 'bedridden', 'bedspring', 'bedspread', 'bedsteads', 'beebreads', 'beechnuts', 'beefiness', 'beefsteak', 'beekeeper', 'beelzebub', 'befalling', 'befitting', 'befogging', 'befouling', 'befriends', 'befuddled', 'befuddles', 'begetting', 'begetters', 'beggaring', 'beginning', 'beginners', 'begirding', 'begriming', 'begrudged', 'begrudges', 'beguiling', 'beguilers', 'behaviors', 'beheading', 'behemoths', 'beholding', 'beholders', 'behooving', 'bejeweled', 'belabored', 'belatedly', 'beleaguer', 'believing', 'believers', 'belittled', 'belittles', 'bellicose', 'bellowing', 'bellyache', 'belonging', 'belvedere', 'bemoaning', 'benchmark', 'benefices', 'benefited', 'benighted', 'benignity', 'benignant', 'benthoses', 'benumbing', 'benzoates', 'bequeaths', 'berceuses', 'bereaving', 'bergamots', 'beriberis', 'berkelium', 'bernoulli', 'beryllium', 'beseeched', 'beseeches', 'beseeming', 'besetting', 'besieging', 'besmeared', 'besotting', 'bespangle', 'bespatter', 'bespreads', 'bestirred', 'bestowing', 'bestowals', 'bestrewed', 'bestrides', 'betatrons', 'bethought', 'betokened', 'betraying', 'betrayers', 'betrayals', 'betrothal', 'betrothed', 'bettering', 'beverages', 'bewailing', 'bewilders', 'bewitched', 'bewitches', 'biathlons', 'bicameral', 'bickering', 'biconcave', 'bicuspids', 'bicycling', 'bicyclist', 'bidentate', 'biennials', 'bifurcate', 'bigamists', 'bigotries', 'bilateral', 'bilingual', 'billboard', 'billeting', 'billfolds', 'billhooks', 'billiards', 'billionth', 'billowing', 'bimonthly', 'binderies', 'bindweeds', 'binnacles', 'binocular', 'binomials', 'bioassays', 'biography', 'biologies', 'biologist', 'biorhythm', 'biosphere', 'bipartite', 'birdbaths', 'birdbrain', 'birdcalls', 'birdhouse', 'birdlimes', 'birdseeds', 'birthdays', 'birthmark', 'birthrate', 'bisecting', 'bisection', 'bisectors', 'bisexuals', 'bishopric', 'bisulfate', 'bitchiest', 'bitterest', 'bivalence', 'bivalency', 'bivalents', 'bivouacks', 'blabbiest', 'blabbered', 'blackness', 'blackball', 'blackbird', 'blackbody', 'blackcaps', 'blackdamp', 'blackened', 'blackhead', 'blackjack', 'blacklist', 'blackmail', 'blackouts', 'blacktops', 'blameless', 'blanching', 'blandness', 'blankness', 'blanketed', 'blarneyed', 'blaspheme', 'blasphemy', 'blastoffs', 'blastulas', 'blatantly', 'blathered', 'blazoning', 'bleaching', 'bleachers', 'bleariest', 'blemished', 'blemishes', 'blenching', 'blessings', 'blighting', 'blindness', 'blindfold', 'blistered', 'blizzards', 'blockings', 'blockaded', 'blockades', 'blockages', 'blockhead', 'bloodiest', 'bloodying', 'bloodless', 'bloodline', 'bloodroot', 'bloodshed', 'bloodshot', 'blossomed', 'blotching', 'blowhards', 'blowholes', 'blowpipes', 'blowtorch', 'blowziest', 'blubbered', 'bludgeons', 'bluebells', 'blueberry', 'bluebirds', 'bluegrass', 'bluenoses', 'blueprint', 'blundered', 'blunderer', 'bluntness', 'blurriest', 'blustered', 'boardwalk', 'boathouse', 'boatloads', 'boatswain', 'bobolinks', 'bobtailed', 'bobwhites', 'bodyguard', 'bodysurfs', 'bogginess', 'bohemians', 'boldfaced', 'boldfaces', 'bolivians', 'bollixing', 'bolshevik', 'bolstered', 'bombarded', 'bombasted', 'bombastic', 'bombazine', 'bombproof', 'bombshell', 'bombsight', 'boneblack', 'boneheads', 'bonhomies', 'bonneting', 'boohooing', 'bookcases', 'bookmaker', 'bookmarks', 'bookplate', 'bookracks', 'bookshelf', 'bookstall', 'bookstand', 'bookstore', 'bookworms', 'boomerang', 'boondocks', 'bootblack', 'bootjacks', 'bootlicks', 'bootstrap', 'bordering', 'bordellos', 'borrowing', 'borrowers', 'boskiness', 'bossiness', 'bostonian', 'botanists', 'botanical', 'bothering', 'bottoming', 'botulisms', 'bouillons', 'boulevard', 'bounciest', 'boundless', 'bounteous', 'bountiful', 'bourgeois', 'boutiques', 'bowlegged', 'bowsprits', 'bowstring', 'boycotted', 'boyfriend', 'bracelets', 'bracketed', 'braggarts', 'braidings', 'brainiest', 'brainless', 'brainpans', 'brainwash', 'brainwork', 'branching', 'brandying', 'brashness', 'brassiest', 'brassiere', 'brattiest', 'bravadoes', 'braveries', 'brawniest', 'brazilian', 'breaching', 'breakable', 'breakages', 'breakdown', 'breakfast', 'breakneck', 'breakouts', 'breasting', 'breathing', 'breathers', 'breathier', 'breedings', 'breeziest', 'breezeway', 'brevities', 'breweries', 'briberies', 'brickbats', 'brickwork', 'brickyard', 'briefings', 'briefcase', 'brigadier', 'brightest', 'brightens', 'brilliant', 'brimstone', 'briquette', 'briskness', 'brislings', 'bristling', 'bristlier', 'britannia', 'briticism', 'broaching', 'broadness', 'broadaxes', 'broadcast', 'broadened', 'broadleaf', 'broadloom', 'broadside', 'broadtail', 'brocading', 'broccolis', 'brochette', 'brochures', 'brokerage', 'bromeliad', 'bronchial', 'bronchium', 'broodiest', 'broomcorn', 'brotherly', 'broughams', 'brouhahas', 'browbeats', 'brownouts', 'brunching', 'brunettes', 'brushwood', 'brushwork', 'brusquest', 'brusquely', 'brutality', 'brutalize', 'bryophyte', 'bubbliest', 'bubbletop', 'buccaneer', 'bucharest', 'buckaroos', 'buckboard', 'bucketing', 'buckshots', 'buckskins', 'buckteeth', 'bucktooth', 'buckwheat', 'buddhists', 'budgeting', 'budgetary', 'buffering', 'buffaloes', 'buffeting', 'buildings', 'bulginess', 'bulgarian', 'bulkiness', 'bulkheads', 'bulldozed', 'bulldozer', 'bulldozes', 'bulletins', 'bullfight', 'bullfinch', 'bullfrogs', 'bullheads', 'bullrings', 'bullwhips', 'bulrushes', 'bulwarked', 'bumblebee', 'bumpiness', 'bumptious', 'bungalows', 'bungholes', 'bunkhouse', 'buntlines', 'burdening', 'burgeoned', 'burgesses', 'burliness', 'burlesque', 'burnished', 'burnishes', 'burnooses', 'burrowing', 'bushiness', 'bushwhack', 'butadiene', 'butchered', 'buttering', 'buttercup', 'butterfat', 'butterfly', 'butternut', 'buttoning', 'bypassing', 'bystander', 'byzantine', 'caballero', 'cabinetry', 'cablegram', 'cabochons', 'caboodles', 'cachalots', 'cacophony', 'caesarean', 'cafeteria', 'caffeines', 'cairngorm', 'cakewalks', 'calaboose', 'caladiums', 'calamines', 'calcified', 'calcifies', 'calcimine', 'calcining', 'calculate', 'caledonia', 'calendars', 'calfskins', 'calibrate', 'calipered', 'caliphate', 'callously', 'callboard', 'calliopes', 'callosity', 'callusing', 'calorific', 'calumnies', 'calvinist', 'calvinism', 'cambering', 'cambodian', 'camellias', 'camembert', 'cameramen', 'cameraman', 'camisoles', 'camomiles', 'campaigns', 'campanile', 'campfires', 'camphoric', 'campsites', 'camshafts', 'canadians', 'canailles', 'canalized', 'canalizes', 'canceling', 'cancerous', 'candidacy', 'candidate', 'candlepin', 'canebrake', 'canisters', 'cankering', 'cankerous', 'canniness', 'canneries', 'cannibals', 'cannonade', 'cannoneer', 'canoeists', 'canonical', 'canonized', 'canonizes', 'canopying', 'cantering', 'cantabile', 'canticles', 'cantonese', 'canvassed', 'canvasses', 'capacious', 'capacitor', 'caparison', 'capeskins', 'capillary', 'capriccio', 'capricorn', 'caprioles', 'caprioled', 'capsizing', 'capstones', 'capsuling', 'capsulate', 'captained', 'captaincy', 'captioned', 'captivity', 'captivate', 'capturing', 'capuchins', 'capybaras', 'caracoles', 'carapaces', 'carbolics', 'carbonate', 'carbonize', 'carbonyls', 'carboxyls', 'carbuncle', 'carcasses', 'carcinoma', 'cardamoms', 'cardboard', 'cardigans', 'cardinals', 'cardsharp', 'careering', 'careening', 'carefully', 'caressing', 'caretaker', 'caribbean', 'carillons', 'carnation', 'carnality', 'carnelian', 'carnivals', 'carnivore', 'carnotite', 'carotenes', 'carousing', 'carousals', 'carousels', 'carpenter', 'carpentry', 'carpeting', 'carpetbag', 'carriages', 'carryalls', 'cartilage', 'cartoning', 'cartooned', 'cartridge', 'cartwheel', 'caryatids', 'caryopses', 'caryopsis', 'casanovas', 'cascading', 'casements', 'cashiered', 'cashmeres', 'cassandra', 'casserole', 'cassettes', 'cassowary', 'castanets', 'castaways', 'castigate', 'castrated', 'castrates', 'casuistic', 'casuistry', 'catabolic', 'cataclysm', 'catacombs', 'catalepsy', 'cataloged', 'cataloger', 'catalogue', 'catalyses', 'catalysis', 'catalysts', 'catalytic', 'catalyzed', 'catalyzes', 'catamaran', 'catamount', 'catapults', 'cataracts', 'catarrhal', 'catatonic', 'catatonia', 'catcalled', 'catchiest', 'catchalls', 'catchword', 'catechist', 'catechism', 'catechize', 'catenated', 'catenates', 'caterwaul', 'catharses', 'catharsis', 'cathartic', 'cathedral', 'catheters', 'catholics', 'catnapped', 'cattlemen', 'cattleman', 'caucasian', 'caucasoid', 'caucusing', 'cauldrons', 'causation', 'causality', 'causative', 'causeways', 'cauterize', 'cautioned', 'cavalcade', 'cavaliers', 'cavalries', 'cavernous', 'cavorting', 'ceaseless', 'cecropias', 'celandine', 'celebrity', 'celebrant', 'celebrate', 'celestial', 'celibates', 'celluloid', 'cellulose', 'cementing', 'cenobites', 'cenobitic', 'cenotaphs', 'censoring', 'censorial', 'censuring', 'centering', 'centaurus', 'centenary', 'centigram', 'centipede', 'centrists', 'centrally', 'centriole', 'centroids', 'centuries', 'centurion', 'ceramists', 'cerements', 'cerebrate', 'cerebrums', 'cerecloth', 'certainly', 'certainty', 'certified', 'certifies', 'certitude', 'ceruleans', 'cessation', 'cesspools', 'cetaceans', 'ceylonese', 'chaffered', 'chaffinch', 'chagrined', 'chalkiest', 'challenge', 'challises', 'chambered', 'chambrays', 'chameleon', 'chamfered', 'champagne', 'champions', 'chanciest', 'chandlers', 'chandlery', 'channeled', 'chanteuse', 'chanukahs', 'chaparral', 'chapbooks', 'chaperons', 'chaplains', 'chariness', 'charities', 'charabanc', 'character', 'charcoals', 'charismas', 'charlatan', 'charlotte', 'chartered', 'charwomen', 'charwoman', 'chassidic', 'chassidim', 'chastened', 'chastised', 'chastises', 'chasubles', 'chattiest', 'chattered', 'chauffeur', 'cheapness', 'cheapened', 'checkable', 'checkered', 'checkbook', 'checkmate', 'checkroom', 'checksums', 'cheekiest', 'cheekbone', 'cheeriest', 'cheerless', 'chessiest', 'chemicals', 'chemistry', 'chemurgic', 'chenilles', 'cherished', 'cherishes', 'chestnuts', 'chevalier', 'chicagoan', 'chicanery', 'chickadee', 'chickened', 'chickpeas', 'chickweed', 'chicories', 'chieftain', 'chihuahua', 'chilblain', 'childhood', 'childless', 'childlike', 'chilliest', 'chinatown', 'chinaware', 'chintzier', 'chipboard', 'chipmunks', 'chiropody', 'chirruped', 'chiseling', 'chitchats', 'chittered', 'chivalric', 'chlorates', 'chlordane', 'chlorides', 'chlorines', 'chlorites', 'chloroses', 'chlorosis', 'chocolate', 'choirboys', 'chokedamp', 'choosiest', 'chophouse', 'choppiest', 'chordates', 'chorister', 'chortling', 'chorusing', 'christens', 'christian', 'christmas', 'chromates', 'chromatic', 'chromatin', 'chromites', 'chromiums', 'chronicle', 'chrysalid', 'chrysalis', 'chubbiest', 'chuckling', 'chummiest', 'chunkiest', 'churchmen', 'churchman', 'chutzpahs', 'cicerones', 'cigarette', 'cigarillo', 'cinchonas', 'cinctured', 'cinctures', 'cinematic', 'cinnabars', 'cinnamons', 'ciphering', 'circuited', 'circuitry', 'circulars', 'circulate', 'cirrhoses', 'cirrhosis', 'cirrhotic', 'cirripeds', 'cisternal', 'citations', 'citifying', 'citizenry', 'civilians', 'civilized', 'civilizes', 'clabbered', 'claimants', 'clambered', 'clambakes', 'clammiest', 'clamoring', 'clamorous', 'clamshell', 'clapboard', 'claptraps', 'clarities', 'clarified', 'clarifies', 'clarinets', 'classiest', 'classical', 'classless', 'classmate', 'classroom', 'clattered', 'clavicles', 'cleanness', 'cleanings', 'cleansing', 'cleansers', 'clearness', 'clearings', 'clearance', 'cleavages', 'clenching', 'clergymen', 'clergyman', 'clericals', 'cleveland', 'cleverest', 'clientele', 'climactic', 'climaxing', 'climbable', 'clinching', 'clinchers', 'clinician', 'clipboard', 'clippings', 'cloakroom', 'clobbered', 'clockwise', 'clockwork', 'cloisonne', 'cloisters', 'cloistral', 'closeness', 'closeting', 'clothiers', 'cloudiest', 'cloudless', 'clubbiest', 'clubhouse', 'clumsiest', 'clustered', 'clutching', 'cluttered', 'coadjutor', 'coagulant', 'coagulase', 'coagulate', 'coalesced', 'coalesces', 'coalition', 'coarsened', 'coastline', 'coastwise', 'coatrooms', 'coattails', 'cobwebbed', 'cochineal', 'cockiness', 'cockading', 'cockatoos', 'cockcrows', 'cockerels', 'cockfight', 'cocklebur', 'cockroach', 'cockscomb', 'cocktails', 'codifying', 'coenzymes', 'coercions', 'coercible', 'coexisted', 'coextends', 'coffeepot', 'cofferdam', 'cogencies', 'cogitated', 'cogitates', 'cogitator', 'cognition', 'cognitive', 'cognizant', 'cognomens', 'cogwheels', 'cohabited', 'coherence', 'coherency', 'cohesions', 'coiffeurs', 'coiffures', 'coincided', 'coincides', 'colanders', 'coleslaws', 'coliseums', 'colitises', 'collagens', 'collapsed', 'collapses', 'collaring', 'collating', 'collation', 'collators', 'colleague', 'collected', 'collector', 'collegian', 'colliding', 'collinear', 'collision', 'collocate', 'collodion', 'colloidal', 'colloquia', 'colluding', 'collusion', 'collusive', 'colombian', 'colonists', 'colonelcy', 'colonials', 'colonized', 'colonizer', 'colonizes', 'colonnade', 'colophons', 'colorists', 'colorings', 'colorants', 'colorcast', 'colorfast', 'colorless', 'columbian', 'columbine', 'columbium', 'columnist', 'combating', 'combative', 'combatant', 'combining', 'combinate', 'combusted', 'comebacks', 'comedians', 'comedowns', 'comeliest', 'comforted', 'comforter', 'comically', 'commanded', 'commander', 'commandos', 'commenced', 'commences', 'commended', 'commensal', 'commented', 'commerces', 'commingle', 'commissar', 'committed', 'committal', 'committee', 'commixing', 'commodity', 'commodore', 'commoners', 'commonest', 'commotion', 'communing', 'communion', 'communism', 'community', 'communist', 'communize', 'commuting', 'commuters', 'compacted', 'compactor', 'compactly', 'compadres', 'companies', 'companion', 'comparing', 'compassed', 'compasses', 'compelled', 'compendia', 'conpeting', 'competent', 'compiling', 'compilers', 'complains', 'complaint', 'completed', 'completes', 'complexes', 'compliant', 'complying', 'component', 'comported', 'composing', 'composers', 'composite', 'composure', 'compounds', 'comprised', 'comprises', 'computing', 'computers', 'concavity', 'concealed', 'conceding', 'conceited', 'conceived', 'conceiver', 'conceives', 'concerned', 'concerted', 'concertos', 'concierge', 'conclaves', 'concluded', 'concludes', 'concocted', 'concordat', 'concourse', 'concreted', 'concretes', 'concubine', 'concurred', 'condemned', 'condensed', 'condenser', 'condenses', 'condiment', 'condigned', 'condition', 'condoling', 'condoning', 'conducing', 'conducive', 'conducted', 'conductor', 'conferees', 'conferred', 'conferral', 'confessed', 'confesses', 'confessor', 'confiding', 'confidant', 'confident', 'configure', 'confining', 'confirmed', 'conflicts', 'confluent', 'conformed', 'conformer', 'confounds', 'confreres', 'confronts', 'confucian', 'confucius', 'confusing', 'confusion', 'confuting', 'congealed', 'congeners', 'congenial', 'congeries', 'congested', 'congolese', 'congruity', 'congruous', 'congruent', 'conjoined', 'conjugate', 'conjuring', 'conjurers', 'connected', 'connector', 'conniving', 'connoting', 'connubial', 'conquered', 'conqueror', 'conquests', 'conscious', 'conscript', 'consensus', 'consented', 'conserved', 'conserves', 'considers', 'consigned', 'consignor', 'consignee', 'consisted', 'consoling', 'consommes', 'consonant', 'consorted', 'consortia', 'conspired', 'conspires', 'constable', 'constancy', 'constants', 'constrain', 'constrict', 'construed', 'construes', 'construct', 'consulate', 'consulted', 'consuming', 'consumers', 'contagion', 'contained', 'container', 'contemned', 'contended', 'contender', 'contented', 'contested', 'continent', 'continued', 'continues', 'continual', 'continuum', 'contorted', 'contoured', 'contracts', 'contrails', 'contralto', 'contrasts', 'contrives', 'contrived', 'contumacy', 'contumely', 'contusing', 'contusion', 'convector', 'convening', 'converged', 'converges', 'conversed', 'converses', 'converted', 'converter', 'convexity', 'conveying', 'conveyers', 'convinces', 'convicted', 'convinced', 'convivial', 'convoking', 'convolute', 'convoying', 'convulsed', 'convulses', 'cookbooks', 'cookeries', 'cookhouse', 'coonhound', 'coonskins', 'cooperage', 'cooperate', 'copartner', 'copiously', 'copolymer', 'copulated', 'copulates', 'copybooks', 'copyright', 'coquettes', 'coquilles', 'cordially', 'cordoning', 'cordovans', 'corduroys', 'cordwoods', 'coriander', 'corkiness', 'corkscrew', 'cormorant', 'cornering', 'corncribs', 'cornetist', 'cornfield', 'cornicing', 'cornmeals', 'cornstalk', 'corollary', 'corporals', 'corporate', 'corporeal', 'corpulent', 'corpuscle', 'corralled', 'corrected', 'corrector', 'correctly', 'correlate', 'corridors', 'corroding', 'corrosion', 'corrosive', 'corrugate', 'corrupted', 'corrupter', 'corseting', 'corsicans', 'cortisone', 'corundums', 'coruscate', 'corvettes', 'cosecants', 'cosigning', 'cosigners', 'cosmetics', 'cosmogony', 'cosmology', 'cosmonaut', 'cosponsor', 'cosseting', 'costarred', 'costliest', 'costuming', 'cotangent', 'cotillion', 'cottagers', 'cottoning', 'cotyledon', 'councilor', 'counseled', 'counselor', 'countable', 'countered', 'countdown', 'countless', 'countries', 'couplings', 'courteous', 'courtesan', 'courtiers', 'courtlier', 'courtroom', 'courtship', 'courtyard', 'couturier', 'covariant', 'covenants', 'coverings', 'coverages', 'coveralls', 'coverlets', 'cowardice', 'coworkers', 'coxswains', 'crabbiest', 'crabgrass', 'crackdown', 'crackling', 'crackpots', 'craftiest', 'craftsmen', 'craftsman', 'craggiest', 'cranberry', 'crankiest', 'crankcase', 'crapulous', 'crapulent', 'crassness', 'crayoning', 'craziness', 'creakiest', 'creamiest', 'creations', 'creatures', 'credences', 'credenzas', 'crediting', 'creditors', 'credulity', 'credulous', 'creepiest', 'cremating', 'cremation', 'cremators', 'crematory', 'creosoted', 'creosotes', 'crepitate', 'crescendo', 'crescents', 'cretinism', 'cretonnes', 'crevasses', 'criminals', 'crimsoned', 'crinkling', 'crinoline', 'crippling', 'crispiest', 'crispness', 'criterion', 'criticism', 'criticize', 'critiqued', 'critiques', 'crocheted', 'crocodile', 'croissant', 'cromlechs', 'crookedly', 'croplands', 'croquette', 'crossings', 'crossbars', 'crossbeam', 'crossbows', 'crossbred', 'crosscuts', 'crossfire', 'crossover', 'crossroad', 'crosswalk', 'crosswise', 'crossword', 'crotchets', 'crotchety', 'crouching', 'croupiers', 'crowfoots', 'crucially', 'crucibles', 'crucified', 'crucifies', 'cruciform', 'crudities', 'cruelties', 'crumbling', 'crummiest', 'crumpling', 'crunching', 'crunchier', 'crusading', 'crusaders', 'crushable', 'crustiest', 'crybabies', 'cryogenic', 'cubbyhole', 'cuckolded', 'cucumbers', 'cuddliest', 'cudgeling', 'cuirasses', 'culminate', 'cultivate', 'culturing', 'cumbering', 'cumbrance', 'cuneiform', 'cunningly', 'cupboards', 'curatives', 'curbstone', 'curettage', 'curfewing', 'curiously', 'curiosity', 'curliness', 'curlicued', 'curlicues', 'currently', 'curricula', 'currycomb', 'curtailed', 'curtained', 'curtsying', 'curvature', 'curvetted', 'cushioned', 'cuspidors', 'cuspidate', 'custodies', 'custodial', 'custodian', 'customers', 'customary', 'customize', 'cutaneous', 'cutlasses', 'cutthroat', 'cuttingly', 'cyanogens', 'cyclamate', 'cyclamens', 'cyclopean', 'cyclorama', 'cyclotron', 'cylinders', 'cynicisms', 'cynically', 'cynosures', 'cypresses', 'cyprinoid', 'cytoplasm', 'cytoplast', 'cytosines', 'czarevnas', 'dachshund', 'dactylics', 'daffiness', 'daffodils', 'daintiest', 'daiquiris', 'dairymaid', 'dalliance', 'dalmatian', 'damascene', 'damnation', 'damnedest', 'dampening', 'damselfly', 'dandelion', 'dandified', 'dandifies', 'dandruffs', 'dangerous', 'daredevil', 'darkening', 'darkrooms', 'darwinian', 'darwinism', 'dashboard', 'dastardly', 'databases', 'datelined', 'datelines', 'daughters', 'dauntless', 'davenport', 'daybreaks', 'daydreams', 'dayflower', 'daylights', 'deaconess', 'deadbeats', 'deadening', 'deadheads', 'deadliest', 'deadlines', 'deadlocks', 'deadwoods', 'deafening', 'deaneries', 'deathbeds', 'deathblow', 'deathless', 'deathlike', 'deathtrap', 'debarking', 'debarring', 'debatably', 'debatable', 'debauched', 'debauches', 'debaucher', 'debenture', 'debriefed', 'debugging', 'debunking', 'debutante', 'decadence', 'decagrams', 'decalcify', 'decalogue', 'decameter', 'decamping', 'decanting', 'decanters', 'decathlon', 'deceasing', 'decedents', 'deceitful', 'deceiving', 'deceivers', 'decencies', 'deception', 'deceptive', 'decidedly', 'deciduous', 'decigrams', 'deciliter', 'decillion', 'decimally', 'decimated', 'decimates', 'decimator', 'decimeter', 'deciphers', 'decisions', 'deckhouse', 'declaimed', 'declaring', 'declining', 'declivity', 'decocting', 'decoction', 'decollete', 'decompose', 'decontrol', 'decorated', 'decorates', 'decorator', 'decrement', 'decreased', 'decreases', 'decreeing', 'decretals', 'decrypted', 'decussate', 'dedicates', 'dedicator', 'dedicated', 'deducibly', 'deducible', 'deducting', 'deduction', 'deductive', 'deepening', 'deerhound', 'deerskins', 'defalcate', 'defaulted', 'defeating', 'defeatist', 'defeatism', 'defecated', 'defecates', 'defecator', 'defecting', 'defection', 'defectors', 'defective', 'defending', 'defenders', 'defendant', 'defensive', 'deferment', 'deference', 'deferring', 'deferrers', 'deferrals', 'defiances', 'defiantly', 'deficient', 'definably', 'definable', 'deflating', 'deflation', 'deflators', 'deflected', 'deflector', 'deflowers', 'defoliant', 'defoliate', 'deforests', 'deforming', 'deformity', 'defrauded', 'defraying', 'defrayals', 'defrocked', 'defrosted', 'defroster', 'degaussed', 'degausses', 'degrading', 'degraders', 'degreased', 'degreases', 'dehiscing', 'dehiscent', 'dehydrate', 'dejecting', 'dejection', 'delegated', 'delegates', 'deletions', 'delftware', 'delicious', 'delighted', 'delimited', 'delimiter', 'delineate', 'delirious', 'deliriums', 'delivered', 'deliverer', 'delousing', 'delusions', 'demagogic', 'demagogue', 'demanding', 'demarcate', 'demarches', 'demeaning', 'demeanors', 'dementias', 'demerited', 'demijohns', 'demimonde', 'demitasse', 'democracy', 'democrats', 'demoniacs', 'demotions', 'demounted', 'demulcent', 'demurring', 'demurrers', 'demurrage', 'demystify', 'denatured', 'denatures', 'dendrites', 'denigrate', 'denitrify', 'denounced', 'denouncer', 'denounces', 'densities', 'dentistry', 'dentition', 'deodorant', 'deodorize', 'deoxidize', 'departing', 'departure', 'depending', 'dependent', 'depicting', 'depiciton', 'deplaning', 'depleting', 'depletion', 'deploring', 'deploying', 'deponents', 'deporting', 'deportees', 'deposable', 'deposited', 'depositor', 'depraving', 'depravity', 'deprecate', 'depredate', 'depressed', 'depresses', 'depressor', 'depriving', 'deprivals', 'deprogram', 'deputized', 'deputizes', 'derailing', 'deranging', 'derelicts', 'derisions', 'derivably', 'derivable', 'derogated', 'derogates', 'derrieres', 'derringer', 'dervishes', 'desalting', 'descanted', 'descended', 'descrying', 'described', 'describes', 'desecrate', 'deserting', 'deserters', 'desertion', 'deserving', 'desiccant', 'desiccate', 'designing', 'designers', 'designate', 'designees', 'desirably', 'desirable', 'desisting', 'desolated', 'desolates', 'despaired', 'desperado', 'desperate', 'despising', 'despoiled', 'despotism', 'destining', 'destinies', 'destitute', 'destroyed', 'destroyer', 'destructs', 'desuetude', 'desultory', 'detaching', 'detailing', 'detaining', 'detecting', 'detection', 'detectors', 'detective', 'detention', 'determent', 'detergent', 'determine', 'deterring', 'deterrent', 'detesting', 'dethroned', 'dethrones', 'detonable', 'detonated', 'detonates', 'detonator', 'detouring', 'detracted', 'detractor', 'detrained', 'detriment', 'deuterium', 'deuterons', 'devaluing', 'devaluate', 'devastate', 'developed', 'developer', 'deviances', 'deviating', 'deviation', 'deviators', 'devilment', 'devilfish', 'devisable', 'devolving', 'devotedly', 'devotions', 'devouring', 'dexedrine', 'dexterity', 'dexterous', 'dextroses', 'diabetics', 'diacritic', 'diagnosed', 'diagnoses', 'diagnosis', 'diagonals', 'dialectal', 'dialectic', 'dialoging', 'dialogued', 'dialogues', 'dialyzing', 'diameters', 'diametric', 'diapering', 'diapasons', 'diaphragm', 'diarrheas', 'diastases', 'diastasic', 'diastoles', 'diastolic', 'diathermy', 'diatomite', 'diatribes', 'diazepams', 'dichotomy', 'dickering', 'diclinous', 'dictation', 'dictating', 'dictators', 'didymiums', 'dietetics', 'dietitian', 'differing', 'different', 'difficult', 'diffident', 'diffracts', 'diffusing', 'diffusion', 'diffusive', 'diffusely', 'digesting', 'digestion', 'digestive', 'digitalis', 'digitized', 'digitizes', 'dignities', 'dignified', 'dignifies', 'dignitary', 'digressed', 'digresses', 'dihedrals', 'dilations', 'dilatable', 'diligence', 'dilutions', 'dimension', 'dimorphic', 'dimwitted', 'dinginess', 'dinosaurs', 'diphthong', 'diplomacy', 'diplomats', 'dipsticks', 'dipterous', 'directing', 'direction', 'directive', 'directors', 'directory', 'direfully', 'dirigible', 'dirtiness', 'disabling', 'disabused', 'disabuses', 'disaffect', 'disaffirm', 'disagrees', 'disagreed', 'disallows', 'disappear', 'disarming', 'disarrays', 'disasters', 'disavowed', 'disavowal', 'disbanded', 'disbarred', 'disbelief', 'disbudded', 'disburden', 'disbursed', 'disbursal', 'disburses', 'discarded', 'discerned', 'discharge', 'disciples', 'disclaims', 'disclosed', 'discloses', 'discolors', 'discomfit', 'discorded', 'discounts', 'discourse', 'discovers', 'discovery', 'discredit', 'discussed', 'discusses', 'disdained', 'diseasing', 'disembark', 'disembody', 'disengage', 'disesteem', 'disfavors', 'disfigure', 'disgorged', 'disgorges', 'disgraced', 'disgraces', 'disguised', 'disguises', 'disgusted', 'dishcloth', 'dishevels', 'dishonest', 'dishonors', 'dishtowel', 'dishwater', 'disinfect', 'disinters', 'disjoined', 'disjoints', 'diskettes', 'disliking', 'dislocate', 'dislodged', 'dislodges', 'dismantle', 'dismaying', 'dismember', 'dismissed', 'dismisses', 'dismissal', 'dismounts', 'disobeyed', 'disoblige', 'disorders', 'disorient', 'disowning', 'disparity', 'disparage', 'disparate', 'dispelled', 'dispensed', 'dispenser', 'dispenses', 'dispersed', 'disperses', 'dispersal', 'dispirits', 'displaces', 'displayed', 'displease', 'disported', 'disposing', 'disposals', 'disproofs', 'disproved', 'disproves', 'disputing', 'disputant', 'disquiets', 'disregard', 'disrepair', 'disrepute', 'disrobing', 'disrupted', 'disrupter', 'dissected', 'dissector', 'dissemble', 'dissented', 'dissenter', 'dissevers', 'dissident', 'dissipate', 'dissolute', 'dissolved', 'dissolves', 'dissonant', 'dissurded', 'dissuades', 'distances', 'distanced', 'distantly', 'distastes', 'distemper', 'distended', 'distilled', 'distiller', 'distorted', 'distracts', 'districts', 'distrusts', 'disturbed', 'disulfide', 'disunions', 'disunited', 'disunites', 'dithering', 'diuretics', 'divagated', 'divagates', 'diverging', 'divergent', 'diversion', 'diversity', 'diversify', 'diverting', 'divesting', 'dividable', 'dividends', 'divisibly', 'divisible', 'divisions', 'divorcing', 'divorcees', 'divulging', 'divulgers', 'dixieland', 'dizziness', 'dobermans', 'docketing', 'dockhands', 'dockyards', 'doctoring', 'doctorate', 'doctrines', 'doctrinal', 'documents', 'doddering', 'dodecagon', 'dogfights', 'doggerels', 'doghouses', 'dogmatist', 'dogmatism', 'dolomites', 'domestics', 'domiciles', 'domiciled', 'dominions', 'dominance', 'dominants', 'dominated', 'dominates', 'dominator', 'domineers', 'dominican', 'donations', 'doodlebug', 'doohickey', 'doomsdays', 'doorbells', 'doorjambs', 'doorknobs', 'doornails', 'doorsteps', 'doorstops', 'dormitory', 'dosimeter', 'dotmatrix', 'dottiness', 'doubloons', 'doubtless', 'doughiest', 'doughboys', 'doughnuts', 'doughtier', 'dovecotes', 'dovetails', 'dowdiness', 'downiness', 'downbeats', 'downfalls', 'downgrade', 'downloads', 'downpours', 'downrange', 'downright', 'downslide', 'downstage', 'downswing', 'downtimes', 'downtowns', 'downturns', 'downwards', 'dracaenas', 'draconian', 'draftiest', 'draftsmen', 'draftsman', 'draggling', 'dragonfly', 'dragooned', 'drainages', 'drainpipe', 'dramamine', 'dramatist', 'dramatics', 'dramatize', 'draperies', 'drawbacks', 'drawknife', 'dreamiest', 'dreamland', 'dreamlike', 'dreariest', 'drenching', 'dressiest', 'dressings', 'dressages', 'dribbling', 'driftwood', 'drinkable', 'drippiest', 'drippings', 'driveling', 'driveways', 'drizzling', 'drollness', 'dromedary', 'droopiest', 'droppings', 'drowsiest', 'druggists', 'drugstore', 'drumbeats', 'drumheads', 'drumstick', 'drunkards', 'drunkenly', 'dualities', 'dualistic', 'dubieties', 'dubiously', 'duchesses', 'duckbills', 'ducklings', 'duckweeds', 'ductility', 'dulcimers', 'dumbbells', 'dumfounds', 'dumpiness', 'dumplings', 'dungarees', 'dunghills', 'duodecimo', 'duplicity', 'duplicate', 'durations', 'duskiness', 'dustiness', 'dutifully', 'dwellings', 'dwindling', 'dyestuffs', 'dynamisms', 'dynamists', 'dynamited', 'dynamites', 'dynasties', 'dysentery', 'dyslectic', 'dyslexics', 'dyslexias', 'dyspepsia', 'dyspeptic', 'dysphasia', 'dystrophy', 'eagerness', 'earmarked', 'earnestly', 'earphones', 'earthiest', 'earthling', 'earthward', 'earthwork', 'earthworm', 'easements', 'eastbound', 'eastwards', 'easygoing', 'eavesdrop', 'ebullient', 'eccentric', 'eclectics', 'eclipsing', 'ecliptics', 'ecologies', 'ecologist', 'economies', 'economist', 'economics', 'economize', 'ecosystem', 'ecstasies', 'ectoderms', 'ectoplasm', 'ecumenism', 'edelweiss', 'edentates', 'edibility', 'editorial', 'educating', 'educators', 'education', 'effecting', 'effective', 'effectual', 'efferents', 'efficient', 'effluence', 'effluents', 'effluvial', 'effluvium', 'effulgent', 'effusions', 'eggbeater', 'eggplants', 'eggshells', 'eglantine', 'egomaniac', 'egotistic', 'egregious', 'egressing', 'egyptians', 'eiderdown', 'eighteens', 'eightieth', 'ejaculate', 'ejections', 'elaborate', 'elbowroom', 'elections', 'electives', 'electoral', 'electrify', 'electrode', 'electrons', 'elegances', 'elegantly', 'elegizing', 'elemental', 'elephants', 'elevating', 'elevation', 'elevators', 'elevenths', 'eliciting', 'elicitors', 'eligibles', 'eliminate', 'elkhounds', 'ellipsoid', 'elocution', 'elongated', 'elongates', 'elopement', 'eloquence', 'elsewhere', 'elucidate', 'emaciated', 'emaciates', 'emanating', 'emanation', 'emanative', 'embalming', 'embalmers', 'embanking', 'embargoed', 'embargoes', 'embarking', 'embarrass', 'embassies', 'embattled', 'embattles', 'embedding', 'embellish', 'embezzled', 'embezzler', 'embezzles', 'embitters', 'emblazons', 'embodying', 'embolisms', 'emboldens', 'embosomed', 'embossing', 'embowered', 'embracing', 'embrasure', 'embroider', 'embroiled', 'embryonic', 'emendator', 'emergence', 'emergency', 'emigrants', 'emigrated', 'emigrates', 'eminences', 'eminently', 'emissions', 'emollient', 'emolument', 'emotional', 'empathies', 'empathize', 'emphasize', 'emphysema', 'empirical', 'employing', 'employers', 'employees', 'emporiums', 'empowered', 'empresses', 'emptiness', 'empyreans', 'emuerates', 'emulating', 'emulation', 'emulators', 'emulsions', 'enactment', 'enameling', 'enamoring', 'encamping', 'encaustic', 'encephala', 'enchained', 'enchanted', 'enchanter', 'enchilada', 'enciphers', 'encircles', 'encircled', 'enclosing', 'enclosure', 'encomiums', 'encompass', 'encounter', 'encourage', 'encrusted', 'encrypted', 'encumbers', 'endangers', 'endearing', 'endeavors', 'endlessly', 'endoblast', 'endocarps', 'endocrine', 'endoderms', 'endomorph', 'endoplasm', 'endorsing', 'endorsers', 'endorsees', 'endowment', 'endplates', 'endpoints', 'endurably', 'endurable', 'endurance', 'energetic', 'energized', 'energizer', 'energizes', 'enervated', 'enervates', 'enervator', 'enfeebled', 'enfeebles', 'enfiladed', 'enfilades', 'enfolding', 'enforcing', 'enforcers', 'engenders', 'engineers', 'engirding', 'englander', 'engorging', 'engrafted', 'engraving', 'engravers', 'engrossed', 'engrosses', 'engulfing', 'enhancing', 'enigmatic', 'enjoining', 'enjoinder', 'enjoyably', 'enjoyable', 'enjoyment', 'enkindles', 'enkindled', 'enlarging', 'enlargers', 'enlighten', 'enlisting', 'enlivened', 'enmeshing', 'ennobling', 'enplaning', 'enrapture', 'enriching', 'enrolling', 'ensconces', 'ensconced', 'ensembles', 'ensembled', 'enshrined', 'enshrines', 'enshrouds', 'ensilaged', 'ensilages', 'enslaving', 'ensnaring', 'entailing', 'entangled', 'entangles', 'enteritis', 'entertain', 'enthralls', 'enthroned', 'enthrones', 'enthusing', 'entitling', 'entombing', 'entourage', 'entrances', 'entrained', 'entranced', 'entrapped', 'entreated', 'entropies', 'entrusted', 'entryways', 'entwining', 'enumerate', 'enunciate', 'enveloped', 'envelopes', 'envenomed', 'enviously', 'envisaged', 'envisages', 'envisions', 'enwrapped', 'enzymatic', 'ephedrine', 'ephemeral', 'ephemeras', 'epicenter', 'epicurean', 'epidemics', 'epidermal', 'epidermis', 'epigraphs', 'epileptic', 'epilogues', 'epiphytes', 'epiphytic', 'episcopal', 'epithelia', 'epitomize', 'epizootic', 'eponymous', 'equalized', 'equalizes', 'equations', 'equerries', 'equinoxes', 'equipment', 'equipages', 'equipoise', 'equipping', 'equitably', 'equitable', 'equivocal', 'eradicate', 'erections', 'erogenous', 'eroticism', 'erroneous', 'errorless', 'erstwhile', 'erudition', 'eruptions', 'escalated', 'escalates', 'escalator', 'escapists', 'escapisms', 'escapable', 'escapades', 'escargots', 'escaroles', 'eschewing', 'escorting', 'esophagus', 'espaliers', 'esperanto', 'espionage', 'esplanade', 'espousing', 'espousals', 'espressos', 'essential', 'establish', 'esteeming', 'esthetics', 'estimably', 'estimable', 'estimated', 'estimates', 'estimator', 'estonians', 'estranged', 'estranges', 'estrogens', 'estuaries', 'etceteras', 'eternally', 'ethically', 'ethiopian', 'ethnicity', 'ethnology', 'ethylenes', 'etiolated', 'etiolates', 'etiologic', 'etiquette', 'etruscans', 'etymology', 'eucharist', 'euclidean', 'eulogized', 'eulogizes', 'euphemism', 'euphonies', 'euphonium', 'euphorias', 'europeans', 'euthenics', 'evacuates', 'evacuator', 'evaluated', 'evaluates', 'evaluator', 'evanesced', 'evanesces', 'evangelic', 'evaporate', 'evasively', 'eventides', 'eventuate', 'everglade', 'evergreen', 'everybody', 'evictions', 'evidences', 'evidenced', 'evidently', 'evildoing', 'evildoers', 'evincible', 'evocation', 'evocative', 'evolution', 'exactions', 'exactness', 'examining', 'examiners', 'excavated', 'excavates', 'excavator', 'exceeding', 'excelling', 'excellent', 'excelsior', 'excepting', 'exception', 'excerpted', 'excessive', 'exchanged', 'exchanger', 'exchanges', 'exchequer', 'excisions', 'excitably', 'excitable', 'excitedly', 'exclaimed', 'excluding', 'exclusion', 'exclusive', 'excoriate', 'excrement', 'excreting', 'excretion', 'excretory', 'exculpate', 'excursive', 'excursion', 'excusably', 'excusable', 'execrably', 'execrable', 'execrated', 'execrates', 'execrator', 'executing', 'executors', 'execution', 'executive', 'executrix', 'exemplars', 'exemplary', 'exemplify', 'exempting', 'exemption', 'exercised', 'exerciser', 'exercises', 'exercycle', 'exertions', 'exfoliate', 'exhausted', 'exhibited', 'exhibitor', 'exhorting', 'existence', 'existents', 'exocrines', 'exogenous', 'exonerate', 'exorcists', 'exorcisms', 'exorcised', 'exorciser', 'exorcises', 'exosphere', 'expanding', 'expanders', 'expansive', 'expansion', 'expatiate', 'expecting', 'expectant', 'expedient', 'expedited', 'expediter', 'expedites', 'expelling', 'expending', 'expensing', 'expensive', 'expertise', 'expiating', 'expiation', 'expiators', 'expiatory', 'explained', 'expletive', 'explicate', 'exploding', 'exploited', 'exploiter', 'exploring', 'explorers', 'explosion', 'explosive', 'exponents', 'exporting', 'exporters', 'expositor', 'exposures', 'expounded', 'expressed', 'expresses', 'expressly', 'expulsion', 'expunging', 'expurgate', 'exquisite', 'extempore', 'extending', 'extension', 'extensive', 'extensors', 'extenuate', 'exteriors', 'externals', 'extirpate', 'extolling', 'extollers', 'extorting', 'extortion', 'extracted', 'extractor', 'extradite', 'extremist', 'extremism', 'extremity', 'extremely', 'extricate', 'extrinsic', 'extrovert', 'extruding', 'extrusion', 'extrusive', 'exuberant', 'exudation', 'eyeballed', 'eyelashes', 'eyepieces', 'eyesights', 'eyestalks', 'eyestrain', 'eyewashes', 'fabricate', 'fabulists', 'facetious', 'facsimile', 'factoring', 'factories', 'factorial', 'factotums', 'factually', 'faculties', 'fagotings', 'faineants', 'fairgoers', 'fairyland', 'faithless', 'falconing', 'falconers', 'fallacies', 'fallbacks', 'fallopian', 'fallowing', 'falsities', 'falseness', 'falsehood', 'falsettos', 'falsified', 'falsifier', 'falsifies', 'faltering', 'familiars', 'fanatical', 'fanciness', 'fancywork', 'fandangos', 'fanfaring', 'fanlights', 'fantasies', 'fantasias', 'fantasize', 'fantastic', 'farewells', 'farmhouse', 'farmstead', 'farmyards', 'farragoes', 'farrowing', 'farseeing', 'farthings', 'fascicles', 'fascinate', 'fashioned', 'fastbacks', 'fastening', 'fasteners', 'fatalisms', 'fatalists', 'fathering', 'fathoming', 'fatiguing', 'fattiness', 'fattening', 'fatuities', 'faultiest', 'faultless', 'favorably', 'favorable', 'favorites', 'fearfully', 'feathered', 'featuring', 'fecundity', 'fecundate', 'federally', 'federated', 'federates', 'feedbacks', 'feistiest', 'feldspars', 'felonious', 'feminists', 'feminisms', 'feminines', 'fermented', 'ferneries', 'ferocious', 'ferreting', 'ferrotype', 'ferruling', 'ferryboat', 'fertility', 'fertilize', 'fervently', 'fervidity', 'festering', 'festinate', 'festivity', 'festivals', 'festooned', 'fetishist', 'fetishism', 'fettering', 'feudalist', 'feudalism', 'feudatory', 'fiberfill', 'fiberglas', 'fictional', 'fidgeting', 'fiduciary', 'fieriness', 'fifteenth', 'fiftieths', 'figurines', 'filaments', 'filenames', 'filigrees', 'filigreed', 'filipinos', 'filleting', 'filliping', 'filminess', 'filmgoers', 'filmstrip', 'filtering', 'filthiest', 'filtrated', 'filtrates', 'finagling', 'finalists', 'finalized', 'finalizes', 'financing', 'financial', 'financier', 'finessing', 'fingering', 'fingertip', 'finishing', 'fireballs', 'fireboats', 'fireboxes', 'firebrand', 'firebreak', 'firebrick', 'firedamps', 'fireflies', 'fireguard', 'firehouse', 'firelight', 'fireplace', 'fireplugs', 'firepower', 'fireproof', 'firesides', 'firetraps', 'firewater', 'fireweeds', 'firewoods', 'fireworks', 'firmament', 'firsthand', 'fishiness', 'fishbowls', 'fisheries', 'fishermen', 'fisherman', 'fishhooks', 'fishmeals', 'fishponds', 'fishtails', 'fishwives', 'fissioned', 'fissuring', 'fistulous', 'fixations', 'fixatives', 'flabbiest', 'flagellum', 'flageolet', 'flaggings', 'flagpoles', 'flagrancy', 'flagships', 'flagstaff', 'flagstone', 'flakiness', 'flamencos', 'flameouts', 'flamingos', 'flammable', 'flanneled', 'flapjacks', 'flashiest', 'flashings', 'flashback', 'flashcube', 'flatboats', 'flatirons', 'flattered', 'flattened', 'flatterer', 'flatulent', 'flatworms', 'flaunting', 'flautists', 'flavoring', 'flavorful', 'flaxseeds', 'fledgling', 'fleeciest', 'fleetness', 'fleshiest', 'fleshlier', 'fleshless', 'flextimes', 'flickered', 'flighting', 'flightier', 'flimflams', 'flimsiest', 'flinching', 'flintiest', 'flintlock', 'flippered', 'flippancy', 'floggings', 'floodgate', 'floorings', 'flophouse', 'floppiest', 'flossiest', 'flotation', 'flotillas', 'flouncing', 'flounders', 'flourings', 'flowchart', 'flowerier', 'flowering', 'flowerpot', 'fluctuate', 'fluencies', 'fluffiest', 'flummoxed', 'flummoxes', 'fluoresce', 'fluorides', 'fluorines', 'fluorites', 'flurrying', 'flustered', 'fluttered', 'flyleaves', 'flypapers', 'flyspecks', 'flyweight', 'flywheels', 'foaminess', 'foddering', 'fogginess', 'folderols', 'foliation', 'foliating', 'folklores', 'folksiest', 'follicles', 'following', 'followers', 'fomenting', 'fomenters', 'foodstuff', 'fooleries', 'foolhardy', 'foolishly', 'foolproof', 'foolscaps', 'footballs', 'footboard', 'foorfalls', 'foothills', 'footholds', 'footlight', 'footloose', 'footnoted', 'footnotes', 'footpaths', 'footprint', 'footrests', 'footsteps', 'footstool', 'fopperies', 'forasmuch', 'forbidden', 'forcemeat', 'forearmed', 'forebears', 'foreboded', 'forebodes', 'forebrain', 'forecasts', 'foreclose', 'forecourt', 'foredooms', 'forefront', 'foregoing', 'forehands', 'foreheads', 'foreigner', 'forelimbs', 'forelocks', 'foremasts', 'forenamed', 'forenames', 'forenoons', 'forensics', 'foreparts', 'foresails', 'foreshore', 'foresight', 'foreskins', 'foresting', 'forestall', 'foretaste', 'foretells', 'forethink', 'foretoken', 'forewings', 'forewarns', 'forewords', 'forfeited', 'forgather', 'forgeries', 'forgetful', 'forgetter', 'forgiving', 'forgotten', 'formation', 'formalist', 'formalism', 'formality', 'formalize', 'formative', 'formatted', 'formatter', 'formulaic', 'formulate', 'fornicate', 'forsaking', 'forswears', 'forsythia', 'forthwith', 'fortieths', 'fortified', 'fortifier', 'fortifies', 'fortitude', 'fortnight', 'fortunate', 'forwarded', 'fossilize', 'fostering', 'foundered', 'foundling', 'foundries', 'fountains', 'fourscore', 'foursomes', 'fourteens', 'foxgloves', 'foxhounds', 'fractions', 'fractious', 'fractured', 'fractures', 'fragility', 'fragments', 'fragrance', 'frailties', 'framework', 'franchise', 'francisco', 'frangible', 'frankness', 'frankfort', 'fraternal', 'frazzling', 'freakiest', 'freckling', 'freeboard', 'freeholds', 'freeloads', 'freemason', 'freestone', 'freighted', 'freighter', 'frenchmen', 'frenchman', 'frequency', 'frequents', 'freshness', 'freshened', 'freudians', 'fricassee', 'fricative', 'frictions', 'frightens', 'frightful', 'frigidity', 'frilliest', 'friskiest', 'frittered', 'frivolity', 'frivolous', 'frizziest', 'frizzling', 'froggiest', 'frolicked', 'frolicker', 'frontages', 'frontiers', 'frostiest', 'frostings', 'frostbite', 'frothiest', 'frowziest', 'fructoses', 'frugality', 'fruitiest', 'fruitions', 'fruitcake', 'fruitless', 'frumpiest', 'frustrate', 'fugitives', 'fulfilled', 'fullbacks', 'fulminate', 'fumigated', 'fumigates', 'fumigator', 'functions', 'fundament', 'fungicide', 'funicular', 'funkiness', 'funniness', 'funneling', 'furbelows', 'furbished', 'furbishes', 'furiously', 'furloughs', 'furnished', 'furnishes', 'furniture', 'furriness', 'furrowing', 'furtively', 'furthered', 'fuselages', 'fusiliers', 'fusillade', 'fussiness', 'futurisms', 'futurists', 'fuzziness', 'gabardine', 'gabbiness', 'gadabouts', 'gainfully', 'galantine', 'gallantry', 'galleries', 'gallivant', 'galloping', 'gallstone', 'galvanism', 'galvanize', 'gamboling', 'gamecocks', 'gamesters', 'gammoning', 'gangliest', 'gangplank', 'gangrened', 'gangrenes', 'gangsters', 'gardening', 'gardeners', 'gardenias', 'gargoyles', 'garlanded', 'garmented', 'garnering', 'garnished', 'garnishes', 'garnishee', 'garniture', 'garrisons', 'garroting', 'garrulity', 'garrulous', 'gartering', 'gaslights', 'gasolines', 'gaspingly', 'gastritis', 'gastropod', 'gastrulas', 'gatefolds', 'gateposts', 'gatherers', 'gathering', 'gaucherie', 'gaudiness', 'gauntlets', 'gauziness', 'gawkiness', 'gazetting', 'gazetteer', 'gearboxes', 'gearshift', 'gearwheel', 'gemstones', 'gendarmes', 'genealogy', 'generally', 'generated', 'generates', 'generator', 'geniality', 'genitalia', 'genitives', 'genocides', 'genocidal', 'genotypes', 'genotypic', 'gentility', 'gentleman', 'gentlemen', 'genuflect', 'genuinely', 'geodesies', 'geodesics', 'geography', 'geologies', 'geologist', 'geometers', 'geometric', 'georgette', 'geotactic', 'geotropic', 'geraniums', 'geriatric', 'germanies', 'germanium', 'germicide', 'germinant', 'germinate', 'gerundive', 'gestating', 'gestation', 'gesturing', 'ghanaians', 'ghastlier', 'ghostlier', 'ghostlike', 'gibbering', 'gibberish', 'gibbeting', 'gibraltar', 'giddiness', 'gigantism', 'gimcracks', 'gimleting', 'gimmicked', 'gimmickry', 'gingering', 'girlishly', 'giveaways', 'gladdened', 'gladiator', 'gladiolus', 'glamorize', 'glamorous', 'glandular', 'glaringly', 'glassiest', 'glassfuls', 'glassines', 'glassware', 'glasswork', 'glaucomas', 'gleanings', 'gleefully', 'glimmered', 'glimpsing', 'glissaded', 'glissades', 'glissandi', 'glissando', 'glistered', 'glistened', 'glittered', 'gloamings', 'globefish', 'globulins', 'gloomiest', 'glorified', 'glorifies', 'glossiest', 'glottises', 'glowering', 'glowworms', 'gloxinias', 'glutamate', 'glutinous', 'glycerins', 'glycerols', 'glycogens', 'goatskins', 'goddesses', 'godfather', 'godliness', 'godmother', 'godparent', 'goldbrick', 'goldenest', 'goldenrod', 'goldfinch', 'goldsmith', 'gondolier', 'gonfalons', 'gonococci', 'gonorrhea', 'goodliest', 'goofiness', 'gooseneck', 'gossamers', 'gossiping', 'goulashes', 'gourmands', 'goutiness', 'governing', 'governors', 'governess', 'graceless', 'gradation', 'gradating', 'gradients', 'gradually', 'graduated', 'graduates', 'graftings', 'grainiest', 'grampuses', 'granaries', 'grandness', 'grandaunt', 'granddads', 'grandeurs', 'grandiose', 'grandsons', 'granulate', 'granulize', 'grapeshot', 'grapevine', 'graphemes', 'graphites', 'graphitic', 'grappling', 'grassiest', 'grassland', 'gratified', 'gratifies', 'gratingly', 'gratitude', 'gravities', 'graveling', 'graveyard', 'gravitate', 'gravitons', 'graybeard', 'greasiest', 'greatness', 'greatcoat', 'greediest', 'greenness', 'greenings', 'greenback', 'greenbelt', 'greengage', 'greenhorn', 'greenland', 'greenroom', 'greenwood', 'greetings', 'gregorian', 'grenading', 'grenadier', 'grenadine', 'greyhound', 'griddling', 'gridirons', 'grievance', 'grillroom', 'griminess', 'grimacing', 'grindings', 'grisliest', 'gristlier', 'gristmill', 'grittiest', 'grizzlier', 'grizzlies', 'groceries', 'groggiest', 'grommeted', 'groomsmen', 'groomsman', 'grooviest', 'grosbeaks', 'grosgrain', 'grossness', 'grotesque', 'grouching', 'grouchily', 'grouchier', 'grounding', 'groundhog', 'groundnut', 'groupings', 'groveling', 'grubbiest', 'grubstake', 'gruffness', 'grumbling', 'grumblers', 'grumpiest', 'guarantor', 'guarantee', 'guardians', 'guardrail', 'guardroom', 'guardsmen', 'guardsman', 'guatemala', 'gudgeoned', 'guernseys', 'guerrilla', 'guesswork', 'guffawing', 'guidances', 'guidebook', 'guideline', 'guidepost', 'guildhall', 'guileless', 'guiltiest', 'guiltless', 'guitarist', 'gulfweeds', 'gumminess', 'gumptions', 'guncotton', 'gunfights', 'gunmetals', 'gunneries', 'gunpowder', 'gunrunner', 'gunsmiths', 'gusseting', 'gustiness', 'gustatory', 'gutlessly', 'gutsiness', 'guttering', 'gutturals', 'gymnasium', 'gyrations', 'gyrfalcon', 'gyroscope', 'habitably', 'habitable', 'habituate', 'habitudes', 'haciendas', 'hackneyed', 'haggardly', 'hailstone', 'hailstorm', 'hairiness', 'hairbrush', 'haircloth', 'hairlines', 'halations', 'halfbacks', 'halftones', 'halitoses', 'halitosis', 'hallmarks', 'hallooing', 'hallowing', 'halloween', 'halophyte', 'haltering', 'haltingly', 'hamburger', 'hammering', 'hampering', 'hampshire', 'hamstring', 'hamstrung', 'handiness', 'handballs', 'handbills', 'handbooks', 'handcarts', 'handclasp', 'handcuffs', 'handicaps', 'handiwork', 'handlebar', 'handmaids', 'handrails', 'handshake', 'handsomer', 'handstand', 'hangnails', 'hangovers', 'hankering', 'haphazard', 'happiness', 'happening', 'harangued', 'haranguer', 'harangues', 'harassing', 'harbinger', 'harboring', 'hardiness', 'hardbacks', 'hardballs', 'hardboard', 'hardening', 'hardihood', 'hardships', 'hardtacks', 'hardwired', 'hardwires', 'hardwoods', 'harlequin', 'harmonies', 'harmonics', 'harmonica', 'harmonium', 'harmonize', 'harnessed', 'harnesses', 'harpooned', 'harquebus', 'harridans', 'harrowing', 'harshness', 'harvested', 'harvester', 'hastiness', 'hastening', 'hatchback', 'hatcheted', 'hatchways', 'haughtily', 'haughtier', 'haversack', 'hawaiians', 'hawksbill', 'hawkweeds', 'hawthorns', 'haystacks', 'hazarding', 'hazardous', 'hazelnuts', 'headiness', 'headaches', 'headbands', 'headboard', 'headdress', 'headfirst', 'headgears', 'headlands', 'headlight', 'headlined', 'headliner', 'headlines', 'headlocks', 'headphone', 'headpiece', 'headrests', 'headstall', 'headstand', 'headstock', 'headstone', 'healthily', 'healthier', 'healthful', 'hearkened', 'heartiest', 'heartache', 'heartbeat', 'heartburn', 'heartened', 'heartfelt', 'heartland', 'heartless', 'heartsick', 'heartwood', 'heaviness', 'hecatombs', 'hectogram', 'hectoring', 'hedgehogs', 'hedgehops', 'hedgerows', 'hedonisms', 'hedonists', 'heehawing', 'heftiness', 'heightens', 'heiresses', 'heirlooms', 'heliports', 'hellebore', 'hellfires', 'hellholes', 'helpfully', 'helpmates', 'helpmeets', 'hematites', 'hemostats', 'hemstitch', 'henpecked', 'hepaticas', 'hepatitis', 'heptagons', 'heralding', 'heraldist', 'herbalist', 'herbarium', 'herbicide', 'herbivore', 'herculean', 'hereabout', 'hereafter', 'herefords', 'heretical', 'heritably', 'heritable', 'heritages', 'hermitage', 'hesitancy', 'hesitated', 'hesitater', 'hesitates', 'heterodox', 'heteronym', 'heuristic', 'hexagonal', 'hexagrams', 'hexameter', 'hibernate', 'hiccupped', 'hickories', 'hideously', 'hideaways', 'hidebound', 'hierarchy', 'highballs', 'highbrows', 'highchair', 'highlands', 'highlight', 'highroads', 'hightails', 'hijacking', 'hijackers', 'hilarious', 'hilliness', 'hillbilly', 'hillsides', 'hindering', 'hindmosts', 'hindrance', 'hindsight', 'hirelings', 'hispanics', 'histamine', 'histogram', 'histology', 'histories', 'historian', 'hitchhike', 'hoarfrost', 'hobbyists', 'hobgoblin', 'hobnailed', 'hobnobbed', 'hogsheads', 'hogwashes', 'holdovers', 'hollering', 'hollander', 'hollowing', 'hollyhock', 'hollywood', 'holocaust', 'holograms', 'holograph', 'holsteins', 'holystone', 'homegrown', 'homeliest', 'homelands', 'homemaker', 'homeopath', 'homeowner', 'homerooms', 'homespuns', 'homestead', 'homicides', 'homicidal', 'homiletic', 'hominoids', 'homograph', 'homologue', 'homonymic', 'homophile', 'homophone', 'homophony', 'homunculi', 'honchoing', 'hondurans', 'honesties', 'honeybees', 'honeycomb', 'honeydews', 'honeymoon', 'honorably', 'honorable', 'honoraria', 'honorific', 'hoodwinks', 'hookworms', 'hooligans', 'hoosegows', 'hopefully', 'hopscotch', 'horehound', 'hornbills', 'hornbooks', 'hornpipes', 'horologer', 'horologes', 'horologic', 'horoscope', 'horoscopy', 'horrifies', 'horrified', 'horseback', 'horsecars', 'horsehair', 'horsehide', 'horseplay', 'horseshoe', 'horsetail', 'horsewhip', 'hortative', 'hortatory', 'hosieries', 'hospitals', 'hostesses', 'hostility', 'hotfooted', 'hotheaded', 'hothouses', 'hourglass', 'houseboat', 'housecoat', 'household', 'housemaid', 'housetops', 'housewife', 'housework', 'howitzers', 'howsoever', 'icelandic', 'ichneumon', 'idealists', 'idealisms', 'idealized', 'idealizes', 'ideations', 'identical', 'ideograms', 'ideograph', 'idiomatic', 'idolaters', 'idolizing', 'ignitions', 'ignitable', 'ignorance', 'ignoramus', 'ileitises', 'illegally', 'illegibly', 'illegible', 'illiberal', 'illicitly', 'illnesses', 'illogical', 'illumined', 'illumines', 'illusions', 'imageries', 'imagining', 'imaginary', 'imbalance', 'imbeciles', 'imbroglio', 'imitating', 'imitation', 'imitative', 'imitators', 'immanence', 'immediacy', 'immediate', 'immensity', 'immensely', 'immersing', 'immersion', 'immigrant', 'immigrate', 'imminence', 'immodesty', 'immolated', 'immolates', 'immolator', 'immortals', 'immovably', 'immovable', 'immunized', 'immunizes', 'immutably', 'immutable', 'impacting', 'impaction', 'impairing', 'impaneled', 'imparting', 'impartial', 'impassive', 'impatiens', 'impatient', 'impeached', 'impeaches', 'impedance', 'impelling', 'impellers', 'impending', 'imperfect', 'imperious', 'imperials', 'imperiled', 'impetigos', 'impetuous', 'impetuses', 'impieties', 'impinging', 'impingers', 'implanted', 'implement', 'implicate', 'imploding', 'imploring', 'implosion', 'implosive', 'impolitic', 'importing', 'importers', 'important', 'importune', 'impostors', 'imposture', 'impotence', 'impounded', 'imprecate', 'imprecise', 'impressed', 'impresses', 'imprinted', 'imprisons', 'impromptu', 'improving', 'improvise', 'imprudent', 'impudence', 'impugning', 'impulsion', 'impulsive', 'imputably', 'imputable', 'inability', 'inactions', 'inanities', 'inanimate', 'inaudibly', 'inaudible', 'inaugural', 'incapably', 'incapable', 'incarnate', 'incensing', 'incentive', 'inception', 'inceptive', 'incessant', 'inchworms', 'incidence', 'incidents', 'incipient', 'incisions', 'inclement', 'inclining', 'including', 'inclusion', 'inclusive', 'incognito', 'incommode', 'incorrect', 'increased', 'increases', 'increment', 'incubated', 'incubates', 'incubator', 'incubuses', 'inculcate', 'inculpate', 'incumbent', 'incurably', 'incurable', 'incurious', 'incurring', 'incursion', 'indecency', 'indecorum', 'indelibly', 'indelible', 'indemnity', 'indemnify', 'indenting', 'indenters', 'indention', 'indenture', 'indicated', 'indicates', 'indicator', 'indicting', 'indicters', 'indictors', 'indigence', 'indigents', 'indignity', 'indignant', 'indispose', 'indochina', 'indolence', 'indonesia', 'inducting', 'induction', 'inductive', 'inductors', 'inductees', 'indulging', 'indulgent', 'inebriate', 'inebriety', 'ineffably', 'ineffable', 'inelastic', 'inelegant', 'ineptness', 'infancies', 'infantile', 'infatuate', 'infecting', 'infection', 'infective', 'inferably', 'inferable', 'inference', 'inferiors', 'inferring', 'infertile', 'infesting', 'infielder', 'infinitum', 'infirmity', 'infirmary', 'inflaming', 'inflating', 'inflation', 'inflected', 'inflector', 'inflicted', 'influence', 'influenza', 'informing', 'informers', 'informant', 'infringed', 'infringes', 'infuriate', 'infusions', 'infusible', 'ingenious', 'ingenuity', 'ingenuous', 'ingesting', 'ingestion', 'ingrained', 'ingressed', 'ingresses', 'inhabited', 'inhalants', 'inhalator', 'inherence', 'inherited', 'inheritor', 'inhibited', 'inhibitor', 'initialed', 'initially', 'initiated', 'initiates', 'initiator', 'injecting', 'injection', 'injectors', 'injurious', 'injustice', 'inkstands', 'innermost', 'innkeeper', 'innocence', 'innocents', 'innocuous', 'innovated', 'innovates', 'innovator', 'inoculate', 'inorganic', 'inpatient', 'inputting', 'inquiring', 'inquirers', 'inquiries', 'insatiate', 'inscribed', 'inscriber', 'inscribes', 'insensate', 'inserting', 'insertion', 'insetting', 'insidious', 'insincere', 'insinuate', 'insisting', 'insistent', 'insolence', 'insolubly', 'insoluble', 'insolvent', 'insomnias', 'insomniac', 'inspected', 'inspector', 'inspiring', 'inspirers', 'inspirits', 'instances', 'installed', 'installer', 'instanced', 'instantly', 'instating', 'instigate', 'instilled', 'instincts', 'institute', 'instructs', 'insulated', 'insulates', 'insulator', 'insulting', 'insurable', 'insurance', 'insurgent', 'intaglios', 'integrity', 'integrals', 'integrate', 'intellect', 'intending', 'intendeds', 'intensity', 'intensely', 'intensify', 'intensive', 'intention', 'interment', 'interacts', 'interbred', 'intercede', 'intercept', 'intercoms', 'interdict', 'interests', 'interface', 'interfere', 'interiors', 'interject', 'interlace', 'interlard', 'interleaf', 'interline', 'interlock', 'interlope', 'interlude', 'intermits', 'interning', 'internist', 'internals', 'internees', 'interplay', 'interpose', 'interpret', 'interring', 'interrupt', 'intersect', 'intervals', 'intervene', 'interview', 'interwove', 'intestacy', 'intestate', 'intestine', 'intimated', 'intimates', 'intricacy', 'intricate', 'intrigued', 'intrigues', 'intrinsic', 'introduce', 'introvert', 'intruding', 'intruders', 'intrusion', 'intrusive', 'intuiting', 'intuition', 'intuitive', 'inundated', 'inundates', 'inundator', 'invariant', 'invasions', 'invective', 'inveigles', 'inveighed', 'inveigled', 'inveigler', 'inventing', 'invention', 'inventive', 'inventors', 'inventory', 'inverness', 'inversion', 'inversely', 'inverting', 'investing', 'investors', 'invidious', 'inviolate', 'invisibly', 'invisible', 'invoicing', 'involving', 'irascibly', 'irascible', 'ironbound', 'ironstone', 'ironwoods', 'ironworks', 'irradiate', 'irregular', 'irrigated', 'irrigates', 'irrigator', 'irritably', 'irritable', 'irritants', 'irritated', 'irritates', 'irritator', 'irrupting', 'irruption', 'irruptive', 'isinglass', 'islanders', 'isogamete', 'isolating', 'isolators', 'isolation', 'isomerism', 'isometric', 'isooctane', 'isoprenes', 'isopropyl', 'isosceles', 'isotherms', 'isotropic', 'israelite', 'issuances', 'isthmuses', 'italicize', 'itchiness', 'itemizing', 'iterating', 'iteration', 'iterative', 'itinerant', 'itinerary', 'jabbering', 'jacaranda', 'jackasses', 'jackboots', 'jacketing', 'jackknife', 'jackscrew', 'jackstraw', 'jacquards', 'jailbirds', 'jailbreak', 'jalousies', 'jamaicans', 'jamborees', 'jamestown', 'japanning', 'japonicas', 'jaundiced', 'jaundices', 'jauntiest', 'jawboning', 'jayhawker', 'jaywalked', 'jealously', 'jefferson', 'jellybean', 'jellyfish', 'jeremiads', 'jerkiness', 'jerkwater', 'jerusalem', 'jetliners', 'jettisons', 'jewelries', 'jewelweed', 'jiggering', 'jigsawing', 'jingoisms', 'jingoists', 'jinriksha', 'jitterbug', 'jobberies', 'jobholder', 'jockeying', 'jockstrap', 'jocularly', 'jocundity', 'joineries', 'jointures', 'jokesters', 'jollities', 'jordanian', 'journeyed', 'joviality', 'joylessly', 'jobilance', 'judgeship', 'judgments', 'judicable', 'judicious', 'judiciary', 'juiciness', 'jukeboxes', 'juliennes', 'jumpiness', 'junctions', 'junctures', 'junketing', 'juridical', 'justified', 'justifies', 'juveniles', 'juxtapose', 'kamikazes', 'kangaroos', 'kedgerees', 'keelboats', 'keelhauls', 'keepsakes', 'kenneling', 'kerchiefs', 'kerosenes', 'keyboards', 'keynoting', 'keynoters', 'keystones', 'keystroke', 'kibbutzim', 'kibitzing', 'kibitzers', 'kiboshing', 'kickbacks', 'kickballs', 'kidnaping', 'kidnapers', 'killdeers', 'killifish', 'kilobytes', 'kilocycle', 'kilograms', 'kilohertz', 'kilometer', 'kilowatts', 'kindliest', 'kinescope', 'kingbirds', 'kingbolts', 'kingliest', 'kingships', 'kinkajous', 'kinswomen', 'kinswoman', 'kippering', 'kleenexes', 'knapsacks', 'knaveries', 'kneeholes', 'knifelike', 'knobbiest', 'knockdown', 'knockoffs', 'knockouts', 'knotholes', 'knottiest', 'knottings', 'knowingly', 'knowledge', 'knuckling', 'kookiness', 'kowtowing', 'kymograph', 'laborious', 'laborites', 'laburnums', 'labyrinth', 'lacerated', 'lacerates', 'lacewings', 'lachrymal', 'lacquered', 'lacrosses', 'lactating', 'lactation', 'ladybirds', 'ladyloves', 'ladyships', 'lagniappe', 'lambasted', 'lambastes', 'lambskins', 'lamellate', 'lamenting', 'laminated', 'laminates', 'laminator', 'lampblack', 'lamplight', 'lampooned', 'lampooner', 'lampposts', 'lamcelets', 'landfalls', 'landlords', 'landmarks', 'landowner', 'landscape', 'landslide', 'languages', 'lankiness', 'lanthanum', 'laplander', 'larboards', 'larceners', 'larcenies', 'larcenist', 'larcenous', 'largesses', 'larkspurs', 'larruping', 'laryngeal', 'lassitude', 'latchkeys', 'latecomer', 'latencies', 'laterally', 'lathering', 'latitudes', 'latticing', 'laudatory', 'laughably', 'laughable', 'laughters', 'launching', 'launchers', 'laundered', 'laundries', 'laundress', 'laureates', 'lavaliere', 'lavenders', 'lavishing', 'lawgivers', 'lawmaking', 'lawmakers', 'lawnmower', 'laxatives', 'lazybones', 'leafiness', 'leafstalk', 'leapfrogs', 'leasehold', 'leastwise', 'leavening', 'lecheries', 'lecherous', 'lecithins', 'lecturing', 'lecturers', 'leeriness', 'leftovers', 'legalisms', 'legalists', 'legalized', 'legalizes', 'legations', 'legendary', 'legginess', 'legionary', 'legislate', 'leisurely', 'leitmotif', 'lemoniest', 'lemonades', 'lengthily', 'lengthier', 'lengthens', 'leninists', 'leprosies', 'lessening', 'lethality', 'lethargic', 'lettering', 'letterbox', 'leukocyte', 'levatores', 'leveraged', 'leverages', 'leviathan', 'levitated', 'levitates', 'levitator', 'liability', 'libations', 'liberally', 'liberated', 'liberates', 'liberator', 'liberians', 'liberties', 'libertine', 'libidinal', 'libraries', 'librarian', 'librettos', 'licensing', 'licensers', 'licensees', 'licorices', 'lifeblood', 'lifeboats', 'lifeguard', 'lifelines', 'lifesaver', 'lifestyle', 'lifetimes', 'lifeworks', 'ligaments', 'ligatures', 'lightness', 'lightened', 'lightface', 'lightning', 'lightship', 'lightsome', 'likeliest', 'limbering', 'limburger', 'limelight', 'limericks', 'limestone', 'limitable', 'limitless', 'limonites', 'limousine', 'limpidity', 'linchpins', 'lineament', 'linefeeds', 'lingering', 'lingerers', 'linguists', 'liniments', 'linoleums', 'linotyped', 'linotyper', 'linotypes', 'lionesses', 'lionizing', 'lipsticks', 'liquefied', 'liquefier', 'liquefies', 'liquidity', 'liquidate', 'listening', 'listeners', 'literally', 'literates', 'lithesome', 'lithology', 'lithuania', 'litigants', 'litigated', 'litigates', 'litigator', 'litigious', 'littering', 'litterbug', 'littorals', 'liturgies', 'liveliest', 'liverwort', 'livestock', 'loathings', 'loathsome', 'lobbyists', 'localized', 'localizes', 'locations', 'locatives', 'locksmith', 'locoweeds', 'locutions', 'lodestars', 'lodestone', 'lodgments', 'loftiness', 'logarithm', 'logically', 'logicians', 'logistics', 'logotypes', 'loincloth', 'loitering', 'loiterers', 'lollipops', 'londoners', 'loneliest', 'longboats', 'longevity', 'longhairs', 'longhands', 'longhorns', 'longingly', 'longitude', 'loopholes', 'looseness', 'loosening', 'loquacity', 'lordliest', 'lordships', 'lorgnette', 'lotharios', 'lotteries', 'loudmouth', 'louisiana', 'lousiness', 'lovebirds', 'loveliest', 'lowermost', 'lowliness', 'loyalists', 'loyalties', 'lubricant', 'lubricate', 'lubricous', 'luckiness', 'lucrative', 'lucubrate', 'ludicrous', 'lullabies', 'lumbering', 'luminance', 'lumpiness', 'luncheons', 'lunchroom', 'lustiness', 'lustfully', 'lutherans', 'luxurious', 'luxuriant', 'luxuriate', 'lymphatic', 'lyrebirds', 'lyricists', 'lyricisms', 'macadamia', 'macaroons', 'macerated', 'macerates', 'macerator', 'machining', 'machinist', 'machinery', 'machismos', 'mackinaws', 'macrocosm', 'maddening', 'madhouses', 'madrigals', 'madrilene', 'maelstrom', 'magazines', 'magically', 'magicians', 'magnesium', 'magnetism', 'magnetite', 'magnetize', 'magnetron', 'magnified', 'magnifier', 'magnifies', 'magnitude', 'magnolias', 'maharajah', 'maharanis', 'mailboxes', 'mailgrams', 'mainframe', 'mainlands', 'mainlined', 'mainlines', 'mainmasts', 'mainsails', 'mainsheet', 'mainstays', 'maintains', 'majesties', 'majolicas', 'majorette', 'majuscule', 'makeshift', 'malachite', 'maladroit', 'malamutes', 'malarkeys', 'malaysian', 'malformed', 'malicious', 'maligning', 'malignity', 'malignant', 'malingers', 'malleable', 'maltreats', 'mammalian', 'mammogram', 'manacling', 'manchuria', 'mandarins', 'mandating', 'mandatory', 'mandibles', 'mandolins', 'mandrakes', 'mandrills', 'maneuvers', 'manganese', 'mangroves', 'manhandle', 'manhattan', 'manicotti', 'manicured', 'manicures', 'manifests', 'manifesto', 'manifolds', 'manliness', 'mannequin', 'mannerism', 'manometer', 'manpowers', 'mantillas', 'mantissas', 'marathons', 'marauding', 'marauders', 'marbleize', 'margarine', 'margarita', 'margining', 'marigolds', 'marijuana', 'marinaded', 'marinades', 'marinated', 'marinates', 'mariposas', 'marjorams', 'markdowns', 'marketing', 'marketers', 'marmalade', 'marmoreal', 'marmosets', 'marooning', 'marquetry', 'marquises', 'marriages', 'marshiest', 'marshaled', 'marshland', 'marsupial', 'martinets', 'martyring', 'martyrdom', 'marveling', 'marvelous', 'marzipans', 'masculine', 'masochism', 'masochist', 'masonries', 'massively', 'massacred', 'massacres', 'massaging', 'masseuses', 'mastering', 'masteries', 'masterful', 'mastheads', 'masticate', 'mastodons', 'matchbook', 'matchless', 'matchlock', 'materials', 'maternity', 'matriarch', 'matricide', 'matrimony', 'mattering', 'maturated', 'maturates', 'maundered', 'mauritius', 'mausoleum', 'mavericks', 'mawkishly', 'maxillary', 'maximally', 'maximized', 'maximizes', 'mayflower', 'mayoralty', 'mealtimes', 'meandered', 'meantimes', 'meanwhile', 'measliest', 'measuring', 'measurers', 'meatiness', 'meatballs', 'mechanism', 'mechanics', 'mechanize', 'medallion', 'mediation', 'mediating', 'mediators', 'medically', 'medicated', 'medicates', 'medicines', 'medicinal', 'meditated', 'meditates', 'meditator', 'megabucks', 'megabytes', 'megacycle', 'megahertz', 'megaliths', 'megaphone', 'megaspore', 'megawatts', 'melamines', 'melanesia', 'melanomas', 'meliorate', 'mellowing', 'mellowest', 'melodeons', 'melodious', 'melodrama', 'meltdowns', 'membranes', 'memorably', 'memorable', 'memoranda', 'memorials', 'memorized', 'memorizes', 'menagerie', 'mendacity', 'mendicant', 'mennonite', 'menopause', 'menstrual', 'mentality', 'mentioned', 'mercenary', 'mercerize', 'merchants', 'merciless', 'mercurous', 'mercuries', 'mercurial', 'merganser', 'meridians', 'meringues', 'merriment', 'mescaline', 'mesentery', 'mesmerism', 'mesmerize', 'mesoderms', 'mesquites', 'messiness', 'messenger', 'messianic', 'metabolic', 'metacarpi', 'metalloid', 'metalwork', 'metaphors', 'metaphase', 'metatarsi', 'metazoans', 'meteorite', 'meteoroid', 'methadone', 'methanols', 'methodist', 'metrified', 'metrifies', 'metrology', 'metronome', 'mezzanine', 'mezzotint', 'microchip', 'microcosm', 'microfilm', 'microgram', 'microwave', 'midcourse', 'middlemen', 'middleman', 'midnights', 'midpoints', 'midstream', 'midsummer', 'midwifery', 'midwinter', 'mightiest', 'migraines', 'migrating', 'migration', 'migratory', 'mildewing', 'mileposts', 'milestone', 'militancy', 'militants', 'militated', 'militates', 'milkiness', 'milkmaids', 'milkweeds', 'millenary', 'millennia', 'milliards', 'milligram', 'milliners', 'millinery', 'millionth', 'millipede', 'millraces', 'millstone', 'milwaukee', 'mimesises', 'mimicking', 'mimickers', 'mimicries', 'mincemeat', 'minefield', 'miniature', 'minibuses', 'minidisks', 'minimally', 'minimized', 'minimizes', 'miniscule', 'miniskirt', 'ministers', 'minnesota', 'minstrels', 'minuscule', 'minutemen', 'minuteman', 'mirroring', 'mirthless', 'misbehave', 'miscalled', 'mischance', 'mischiefs', 'miscounts', 'miscreant', 'misdirect', 'misdoings', 'miserably', 'miserable', 'misfeasor', 'misfiring', 'misgiving', 'misgovern', 'misguided', 'misguides', 'mishandle', 'misinform', 'misjudged', 'misjudges', 'mislaying', 'mismanage', 'misnaming', 'misnomers', 'misplaced', 'misplaces', 'misplayed', 'misprints', 'misquoted', 'misquotes', 'misreckon', 'misruling', 'misshaped', 'misshapes', 'misshapen', 'missilery', 'misspells', 'misspends', 'misstated', 'misstates', 'mistiness', 'mistaking', 'mistletoe', 'mistreats', 'mistrials', 'mistrusts', 'mitigated', 'mitigates', 'mitigator', 'mnemonics', 'mobilized', 'mobilizes', 'moccasins', 'mockeries', 'mockingly', 'moderated', 'moderator', 'moderates', 'moderatos', 'modernism', 'modernity', 'modernist', 'modernize', 'modesties', 'modifying', 'modifiers', 'modulated', 'modulates', 'modulator', 'moistness', 'moistened', 'moistures', 'moldiness', 'moldering', 'molecules', 'molecular', 'molehills', 'moleskins', 'molesting', 'mollified', 'mollifies', 'momentous', 'momentary', 'momentums', 'monarchic', 'monastery', 'monatomic', 'monazites', 'moneybags', 'mongering', 'mongolism', 'mongoloid', 'mongooses', 'monitions', 'monitored', 'monkeying', 'monkshood', 'monocular', 'monodists', 'monograms', 'monograph', 'monoliths', 'monologue', 'monomania', 'monomials', 'monoplane', 'monorails', 'monotones', 'monoxides', 'monsignor', 'monstrous', 'monthlies', 'monuments', 'moodiness', 'moonbeams', 'moonlight', 'moonscape', 'moonshine', 'moonstone', 'moralists', 'moralized', 'moralizer', 'moralizes', 'morbidity', 'mordacity', 'mormonism', 'moroccans', 'morphemes', 'morphines', 'mortality', 'mortaring', 'mortgaged', 'mortgagor', 'mortgages', 'mortgagee', 'mortician', 'mortified', 'mortifies', 'mortising', 'mossbacks', 'mothering', 'mothballs', 'motioning', 'motivated', 'motivates', 'motorists', 'motorbike', 'motorboat', 'motorcade', 'motorcars', 'motorized', 'motorizes', 'mountable', 'mountings', 'mountains', 'mousiness', 'mousetrap', 'mouthfuls', 'mouthwash', 'movements', 'mucilages', 'muckraked', 'muckraker', 'muckrakes', 'muddiness', 'mudguards', 'mugginess', 'muleteers', 'mullioned', 'multiform', 'multilith', 'multiples', 'multiplex', 'multitude', 'mummeries', 'mummified', 'mummifies', 'municipal', 'munitions', 'muralists', 'murdering', 'murderers', 'murderous', 'murderess', 'murkiness', 'murmuring', 'muscatels', 'muscovite', 'mushiness', 'mushrooms', 'musicales', 'musically', 'musicians', 'musketeer', 'muskmelon', 'mustiness', 'mustering', 'mustaches', 'mustachio', 'mutations', 'mutilated', 'mutilates', 'mutilator', 'mutinying', 'mutineers', 'muttering', 'mutuality', 'myrmidons', 'mysteries', 'mysticism', 'mystified', 'mystifies', 'mystiques', 'mythology', 'nakedness', 'nameplate', 'namesakes', 'namibians', 'napoleons', 'narcissus', 'narcotics', 'narcotize', 'narrating', 'narration', 'narrators', 'narrative', 'narrowing', 'narrowest', 'nascences', 'nashville', 'nastiness', 'nationals', 'nattiness', 'naturally', 'naughtily', 'naughtier', 'nauseated', 'nauseates', 'navigably', 'navigable', 'navigated', 'navigates', 'navigator', 'nebbishes', 'necessity', 'necessary', 'necklaces', 'necklines', 'neckpiece', 'necrology', 'nectarine', 'neediness', 'nefarious', 'negations', 'negatived', 'negatives', 'neglected', 'neglecter', 'negligees', 'negligent', 'negotiate', 'neighbors', 'nematodes', 'neodymium', 'neolithic', 'neologism', 'neomycins', 'neophytes', 'neoplasms', 'neoprenes', 'nepenthes', 'nephrites', 'nephritis', 'nepotisms', 'neptunium', 'nervation', 'nerveless', 'nervously', 'nestlings', 'networked', 'neuralgia', 'neuralgic', 'neurology', 'neurotics', 'neutering', 'neutrinos', 'nevermore', 'newcomers', 'newlyweds', 'newscasts', 'newsmaker', 'newspaper', 'newsprint', 'newsreels', 'newsstand', 'newtonian', 'nicaragua', 'nicknamed', 'nicknames', 'nicotines', 'nicotinic', 'niftiness', 'nigerians', 'niggardly', 'nightcaps', 'nightclub', 'nightfall', 'nightgown', 'nighthawk', 'nightmare', 'nightspot', 'nighttime', 'nihilisms', 'nihilists', 'nineteens', 'ninetieth', 'nippiness', 'nitrating', 'nitration', 'nitriding', 'nitrified', 'nitrifies', 'nitrogens', 'nocturnes', 'nocturnal', 'noisiness', 'noiseless', 'nominally', 'nominated', 'nominates', 'nominator', 'nonentity', 'nonillion', 'nonjurors', 'nonleaded', 'nonmetals', 'nonpareil', 'nonplused', 'nonpluses', 'nonprofit', 'nonsenses', 'nonsmoker', 'nonverbal', 'nonviable', 'nonvoting', 'noontides', 'noontimes', 'normality', 'normalize', 'normative', 'northeast', 'northerly', 'northland', 'northward', 'northwest', 'norwegian', 'nosebleed', 'nosepiece', 'nostalgia', 'nostalgic', 'notarized', 'notarizes', 'notations', 'notebooks', 'notifying', 'notochord', 'notoriety', 'notorious', 'nourished', 'nourishes', 'novelette', 'novelists', 'novelties', 'novitiate', 'nucleases', 'nucleated', 'nucleates', 'nucleolar', 'nucleolus', 'nucleonic', 'nucleuses', 'nuisances', 'nullities', 'nullified', 'nullifier', 'nullifies', 'numbering', 'numbingly', 'numerable', 'numerated', 'numerates', 'numerator', 'numerical', 'numskulls', 'nunneries', 'nursemaid', 'nurseries', 'nurslings', 'nurturing', 'nutrients', 'nutriment', 'nutrition', 'nutritive', 'nutshells', 'nuttiness', 'obedience', 'obeisance', 'obesities', 'obfuscate', 'objecting', 'objectors', 'objectify', 'objection', 'objective', 'oblations', 'obligated', 'obligates', 'obligator', 'obliquity', 'obliquely', 'oblivions', 'oblivious', 'oblongata', 'obloquies', 'obnoxious', 'obscenity', 'obscenest', 'obscenely', 'obscuring', 'obscurity', 'obscurest', 'obscurant', 'obscurely', 'obsequies', 'observing', 'observers', 'observant', 'obsessing', 'obsession', 'obsessive', 'obsidians', 'obsolesce', 'obstacles', 'obstetric', 'obstinacy', 'obstinate', 'obstructs', 'obtaining', 'obtruding', 'obtrusion', 'obtrusive', 'obverting', 'obviating', 'obviation', 'obviators', 'obviously', 'occasions', 'occipital', 'occluding', 'occlusion', 'occlusive', 'occultist', 'occultism', 'occupying', 'occupancy', 'occupants', 'occupiers', 'occurring', 'occurrent', 'ocotillos', 'octagonal', 'octillion', 'octopuses', 'odalisque', 'odometers', 'offending', 'offenders', 'offensive', 'offerings', 'offertory', 'offhanded', 'officials', 'officiant', 'officiate', 'officious', 'offshoots', 'offspring', 'ohmmeters', 'oilcloths', 'oilstones', 'ointments', 'oleanders', 'oleoresin', 'olfaction', 'olfactory', 'oligarchs', 'oligarchy', 'oligocene', 'oligopoly', 'olympians', 'ombudsmen', 'ombudsman', 'ominously', 'omissions', 'omnibuses', 'omnivores', 'onionskin', 'onlookers', 'onrushing', 'onslaught', 'opacities', 'openworks', 'operating', 'operators', 'operation', 'operative', 'operettas', 'ophidians', 'opponents', 'opportune', 'opposable', 'opposites', 'oppressed', 'oppresses', 'oppressor', 'optically', 'opticians', 'optimisms', 'optimally', 'optimists', 'optimized', 'optimizes', 'optioning', 'optometry', 'opulences', 'orangeade', 'orangutan', 'oratories', 'oratorios', 'orbicular', 'orchestra', 'ordaining', 'orderings', 'orderlies', 'ordinance', 'ordinates', 'ordnances', 'organists', 'organdies', 'organelle', 'organisms', 'organized', 'organizer', 'organizes', 'orgiastic', 'orienting', 'orientals', 'originals', 'originate', 'ornaments', 'orneriest', 'orphaning', 'orphanage', 'orpiments', 'orrisroot', 'orthicons', 'orthodoxy', 'oscillate', 'osculated', 'osculates', 'ossifying', 'osteopath', 'ostracism', 'ostracize', 'ostriches', 'otherwise', 'otologies', 'oubliette', 'ourselves', 'outbidden', 'outboards', 'outbreaks', 'outbursts', 'outdating', 'outermost', 'outfacing', 'outfields', 'outfitted', 'outfitter', 'outflanks', 'outflowed', 'outfoxing', 'outgrowth', 'outhouses', 'outlander', 'outlasted', 'outlawing', 'outlining', 'outliving', 'outnumber', 'outplayed', 'outputted', 'outraging', 'outranked', 'outriders', 'outrigger', 'outshines', 'outsiders', 'outskirts', 'outsmarts', 'outspoken', 'outspread', 'outstayed', 'outstrips', 'outthinks', 'outwardly', 'outweighs', 'outwitted', 'outworked', 'ovenbirds', 'overacted', 'overawing', 'overbites', 'overblown', 'overboard', 'overbooks', 'overcalls', 'overcasts', 'overcoats', 'overcomes', 'overdoing', 'overdosed', 'overdoses', 'overdraft', 'overdraws', 'overdrawn', 'overdress', 'overdrive', 'overdrove', 'overeaten', 'overexert', 'overflows', 'overgrows', 'overgrown', 'overhands', 'overhangs', 'overhauls', 'overheads', 'overhears', 'overheard', 'overheats', 'overjoyed', 'overkills', 'overleaps', 'overloads', 'overlooks', 'overlords', 'overlying', 'overmatch', 'overnight', 'overplays', 'overpower', 'overprint', 'overrated', 'overrates', 'overreach', 'overreact', 'overrides', 'overrules', 'overruled', 'overseers', 'oversells', 'overshoes', 'overshoot', 'oversight', 'oversized', 'overskirt', 'oversleep', 'overslept', 'overspend', 'overspent', 'overstate', 'oversteps', 'overstock', 'overstuff', 'overtakes', 'overtaken', 'overtaxed', 'overtaxes', 'overthrew', 'overthrow', 'overtimes', 'overtones', 'overtrick', 'overtrump', 'overtures', 'overturns', 'overusing', 'overviews', 'overweigh', 'overwhelm', 'overworks', 'overwrite', 'oviparity', 'oviparous', 'ovulating', 'ovulation', 'ownership', 'oxidation', 'oxidative', 'oxidizing', 'oxidizers', 'oxygenate', 'pacemaker', 'pachyderm', 'pacifists', 'pacifisms', 'pacifying', 'pacifiers', 'packaging', 'packsacks', 'paganisms', 'pageantry', 'paginated', 'paginates', 'painfully', 'paintings', 'painterly', 'pakistani', 'palanquin', 'palatably', 'palatable', 'palatines', 'palavered', 'palefaces', 'paleocene', 'paleozoic', 'palestine', 'palinodes', 'palisaded', 'palisades', 'palladium', 'palletize', 'palliated', 'palliates', 'palmettos', 'palmistry', 'palominos', 'palpation', 'palpating', 'palpitate', 'paltering', 'paltriest', 'pampering', 'pamphlets', 'panatelas', 'pancaking', 'pandering', 'pandemics', 'panegyric', 'panelists', 'pangolins', 'panhandle', 'panicking', 'panoplies', 'panoramic', 'panoramas', 'pantalets', 'pantheism', 'pantheist', 'pantheons', 'pantomime', 'pantsuits', 'pantyhose', 'paparazzi', 'paparazzo', 'paperback', 'paperwork', 'papillary', 'papyruses', 'parabolic', 'parabolas', 'parachute', 'paradigms', 'paradises', 'paradoxes', 'paraffins', 'paragraph', 'parakeets', 'paralegal', 'parallels', 'paralyses', 'paralysis', 'paralytic', 'paralyzed', 'paralyzes', 'paramecia', 'paramedic', 'parameter', 'paramount', 'paramours', 'paranoias', 'paranoiac', 'paranoids', 'parasites', 'parasitic', 'parathion', 'parboiled', 'parceling', 'parchment', 'parcheesi', 'pardoning', 'paregoric', 'parenting', 'parentage', 'parietals', 'parisians', 'parlances', 'parlaying', 'parleying', 'parochial', 'parodying', 'paroxysms', 'parqueted', 'parquetry', 'parricide', 'parroting', 'parsimony', 'parsonage', 'partaking', 'parterres', 'partially', 'particles', 'partisans', 'partition', 'partnered', 'partridge', 'passively', 'passbooks', 'passenger', 'passerine', 'passivity', 'passports', 'passwords', 'pastiness', 'pastelist', 'pastiches', 'pastilles', 'pastorals', 'pastorate', 'pastramis', 'pasturing', 'pasturage', 'patchable', 'patchouli', 'patchwork', 'patenting', 'patentees', 'paternity', 'pathogens', 'pathology', 'patiences', 'patiently', 'patriarch', 'patrician', 'patricide', 'patrimony', 'patriotic', 'patrolled', 'patroller', 'patrolmen', 'patrolman', 'patronage', 'patroness', 'patronize', 'pattering', 'patterned', 'paucities', 'paunchier', 'paupering', 'pauperism', 'pauperize', 'pavements', 'pavilions', 'pawnshops', 'paychecks', 'paymaster', 'payrolled', 'peaceably', 'peaceable', 'peacetime', 'peachiest', 'pearliest', 'peasantry', 'peccaries', 'pectorals', 'peculated', 'peculates', 'peculator', 'pecuniary', 'pedagogic', 'pedagogue', 'pedestals', 'pediments', 'pediatric', 'pedicured', 'pedicures', 'pedigreed', 'pedigrees', 'pedometer', 'peepholes', 'peepshows', 'peeresses', 'pegboards', 'pegmatite', 'peignoirs', 'pekingese', 'pellagras', 'pelleting', 'pemmicans', 'penalized', 'penalizes', 'penalties', 'penchants', 'penciling', 'pendulous', 'pendulums', 'penetrate', 'peninsula', 'penitence', 'penitents', 'penknives', 'penniless', 'pensioned', 'pensioner', 'pentacles', 'pentagons', 'pentecost', 'penthouse', 'penumbras', 'penumbrae', 'penurious', 'peppiness', 'peppering', 'pepperoni', 'perceived', 'perceiver', 'perceives', 'perchance', 'percolate', 'perdition', 'peregrine', 'perennial', 'perfected', 'perfectly', 'perfectos', 'perfervid', 'perfidies', 'perforate', 'performed', 'performer', 'perfuming', 'perfumery', 'perfusion', 'perihelia', 'perimeter', 'periphery', 'periscope', 'perishing', 'peritonea', 'perjuring', 'perjurers', 'perjuries', 'perkiness', 'permanent', 'permeably', 'permeable', 'permeated', 'permeates', 'permitted', 'permitter', 'permuting', 'peroxided', 'peroxides', 'perpetual', 'perplexed', 'perplexes', 'persecute', 'persevere', 'persimmon', 'persisted', 'personage', 'personify', 'personnel', 'perspired', 'perspires', 'persuaded', 'persuader', 'persuades', 'pertained', 'pertinent', 'perturbed', 'perusable', 'peruvians', 'pervading', 'pervasion', 'pervasive', 'perverted', 'peskiness', 'pessimism', 'pessimist', 'pestering', 'pesticide', 'pestilent', 'petitions', 'petrified', 'petrifies', 'petroleum', 'petrology', 'pettiness', 'petticoat', 'pettifogs', 'petulance', 'phalanxes', 'phalarope', 'phantasms', 'pharisaic', 'pharisees', 'pharynges', 'pheasants', 'phenomena', 'phenotype', 'philander', 'philately', 'philippic', 'philology', 'phlebitis', 'phoenicia', 'phoenixes', 'phoniness', 'phonemics', 'phonetics', 'phonology', 'phosgenes', 'phosphate', 'phosphors', 'photocell', 'photocopy', 'photostat', 'phrasings', 'physicist', 'physicals', 'physician', 'physicked', 'physiques', 'picayunes', 'pickaxing', 'picketing', 'picklocks', 'picnicked', 'picnicker', 'pictogram', 'pictorial', 'picturing', 'piecemeal', 'piecework', 'piedmonts', 'pietistic', 'piggyback', 'pigheaded', 'pigmented', 'pigtailed', 'pikestaff', 'pilasters', 'pilchards', 'pilfering', 'pilferage', 'pillaging', 'pillboxes', 'pilloried', 'pillories', 'pillowing', 'pimientos', 'pimpernel', 'pinafores', 'pinchbeck', 'pineapple', 'pinnacles', 'pinochles', 'pinpoints', 'pinpricks', 'pinschers', 'pinsetter', 'pinstripe', 'pintsized', 'pinwheels', 'pioneered', 'pipelines', 'piratical', 'pirouette', 'pistachio', 'pitchfork', 'pitchouts', 'pithiness', 'pitifully', 'pittances', 'pituitary', 'pityingly', 'pizzazzes', 'pizzerias', 'pizzicato', 'placating', 'placation', 'placatory', 'placement', 'placental', 'placentas', 'placidity', 'plainness', 'plainsmen', 'plainsman', 'plainsong', 'plaintive', 'plaintiff', 'planarity', 'planarian', 'planetary', 'planetoid', 'planktons', 'plantings', 'plantains', 'plasmatic', 'plasmodia', 'plastered', 'plastrons', 'platefuls', 'platelets', 'platforms', 'platinums', 'platitude', 'platonism', 'plattings', 'plausibly', 'plausible', 'playbacks', 'playbills', 'playfully', 'playgoers', 'playhouse', 'playmates', 'playrooms', 'plaything', 'pleasured', 'pleasures', 'plebeians', 'plectrums', 'plenitude', 'plenteous', 'plentiful', 'pleonasms', 'plethoric', 'plethoras', 'plexiglas', 'pliancies', 'plicating', 'plication', 'plighting', 'plowshare', 'pluckiest', 'plummeted', 'plumpness', 'plundered', 'pluralism', 'plurality', 'pluralist', 'plutocrat', 'plutonium', 'pneumonia', 'pneumonic', 'pocketing', 'pocketful', 'pockmarks', 'poetaster', 'poetesses', 'poignancy', 'pointedly', 'pointless', 'poisoning', 'poisonous', 'pokeberry', 'pokeweeds', 'polarized', 'polarizes', 'polemical', 'polestars', 'policemen', 'policeman', 'polishing', 'politburo', 'political', 'politicks', 'politicos', 'pollinate', 'polliwogs', 'pollsters', 'polluting', 'polluters', 'pollution', 'pollutant', 'pollyanna', 'pollywogs', 'polonaise', 'poltroons', 'polyandry', 'polyester', 'polyglots', 'polygonal', 'polygraph', 'polymeric', 'polynesia', 'polyphony', 'polyvinyl', 'pomanders', 'pommeling', 'pompously', 'pompadour', 'pomposity', 'pondering', 'ponderous', 'ponderosa', 'ponytails', 'poolrooms', 'poorhouse', 'popinjays', 'poppycock', 'populists', 'populaces', 'popularly', 'populated', 'populates', 'porcelain', 'porcupine', 'porpoises', 'porridges', 'porringer', 'portaging', 'portended', 'portfolio', 'portholes', 'porticoes', 'portieres', 'portioned', 'portliest', 'portraits', 'portrayed', 'portrayer', 'portrayal', 'positions', 'positives', 'positrons', 'possessed', 'possesses', 'possessor', 'postboxes', 'postcards', 'postdated', 'postdates', 'posterity', 'posterior', 'posthaste', 'postilion', 'postmarks', 'postnasal', 'postnatal', 'postponed', 'postpones', 'postulant', 'postulate', 'posturing', 'potassium', 'potations', 'potboiler', 'potencies', 'potentate', 'potential', 'pothering', 'potpourri', 'potsherds', 'pottering', 'potteries', 'poultices', 'poultries', 'poundages', 'pourboire', 'poverties', 'powdering', 'powerboat', 'powerless', 'powwowing', 'practices', 'practical', 'pragmatic', 'prankster', 'pratfalls', 'prattling', 'prayerful', 'preaching', 'preachers', 'preambles', 'preceding', 'precedent', 'preceptor', 'precincts', 'precipice', 'precision', 'precisely', 'precluded', 'precludes', 'precocity', 'precooked', 'precursor', 'predating', 'predators', 'predatory', 'predicate', 'predicted', 'predictor', 'prefacing', 'prefacers', 'prefatory', 'preferred', 'prefigure', 'prefixing', 'preflight', 'pregnable', 'pregnancy', 'preheated', 'prejudged', 'prejudges', 'prejudice', 'prelacies', 'preluding', 'premature', 'premiered', 'premieres', 'premising', 'premolars', 'preoccupy', 'preordain', 'preparing', 'prepaying', 'presaging', 'presbyter', 'preschool', 'prescient', 'prescribe', 'prescript', 'presences', 'presented', 'presently', 'preserved', 'preserver', 'preserves', 'preshrunk', 'presiding', 'president', 'presidium', 'presoaked', 'presorted', 'pressings', 'pressroom', 'pressruns', 'pressured', 'pressures', 'presswork', 'prestiges', 'presuming', 'pretended', 'pretender', 'pretenses', 'preterits', 'pretested', 'prettying', 'prettiest', 'prevailed', 'prevalent', 'prevented', 'previewed', 'priceless', 'prickling', 'pricklier', 'priedieus', 'priestess', 'primacies', 'primarily', 'primaries', 'primitive', 'primroses', 'principle', 'principal', 'printable', 'printings', 'printhead', 'printouts', 'prismatic', 'prisoners', 'prissiest', 'privation', 'privacies', 'privateer', 'privately', 'privilege', 'probities', 'probables', 'probating', 'probative', 'probation', 'proboscis', 'procedure', 'proceeded', 'processed', 'processes', 'processor', 'proclaims', 'proconsul', 'procreate', 'proctored', 'procuring', 'procurers', 'proddings', 'prodigies', 'prodigals', 'producing', 'producers', 'profaning', 'profanity', 'professed', 'professes', 'professor', 'proffered', 'profiling', 'profiting', 'profiteer', 'profundos', 'profusion', 'profusely', 'progenies', 'prognoses', 'prognosis', 'prohibits', 'projected', 'projector', 'prolapsed', 'prolixity', 'prologued', 'prologues', 'prolonged', 'promenade', 'prominent', 'promising', 'promoting', 'promoters', 'promotion', 'prompting', 'prompters', 'promptest', 'proneness', 'pronghorn', 'pronounce', 'proofread', 'propagate', 'propelled', 'propeller', 'prophases', 'prophetic', 'proponent', 'proposing', 'proposals', 'propounds', 'propriety', 'propylene', 'prorating', 'proration', 'prorogued', 'prorogues', 'proscenia', 'proscribe', 'prosecute', 'proselyte', 'prosodies', 'prospects', 'prospered', 'prostates', 'prostrate', 'protected', 'protector', 'protested', 'protester', 'protocols', 'prototype', 'protozoan', 'protracts', 'protruded', 'protrudes', 'provencal', 'provender', 'provinces', 'providing', 'provident', 'provision', 'provisory', 'provoking', 'prowesses', 'proximity', 'proximate', 'prudences', 'prudently', 'pruderies', 'prurience', 'prussians', 'psalmists', 'pseudonym', 'psoriases', 'psoriasis', 'psychoses', 'psychosis', 'psychotic', 'ptarmigan', 'pterosaur', 'ptomaines', 'puberties', 'pubescent', 'publicist', 'publicity', 'publicans', 'publicize', 'published', 'publisher', 'publishes', 'puckering', 'pudginess', 'puerility', 'puerperal', 'puffiness', 'puffballs', 'pugilisms', 'pugilists', 'pugnacity', 'puissance', 'pullbacks', 'pullovers', 'pulmonary', 'pulpiness', 'pulpwoods', 'pulsating', 'pulsation', 'pulverize', 'pulverous', 'pummeling', 'punchiest', 'puncheons', 'punctilio', 'punctuate', 'punctured', 'punctures', 'pungently', 'punishing', 'pupations', 'puppeteer', 'purchased', 'purchaser', 'purchases', 'purebreds', 'purgation', 'purgative', 'purgatory', 'purifying', 'purifiers', 'purloined', 'purloiner', 'purported', 'purposing', 'purposely', 'pursuable', 'pursuance', 'purulence', 'purveying', 'purveyors', 'pushiness', 'pushcarts', 'pushovers', 'pussyfoot', 'putrefied', 'putrefies', 'putridity', 'puttering', 'pygmalion', 'pyloruses', 'pyongyang', 'pyorrheas', 'pyramided', 'pyramidal', 'pyrethrum', 'pyridines', 'pyromania', 'quaaludes', 'quadrants', 'quadrates', 'quadratic', 'quadrille', 'quadroons', 'quadruple', 'quadruped', 'quagmires', 'quaintest', 'qualities', 'qualified', 'qualifier', 'qualifies', 'quarrying', 'quarreled', 'quarreler', 'quartered', 'quarterly', 'quarterns', 'quartzite', 'quatrains', 'quavering', 'queasiest', 'quebecois', 'queerness', 'quenching', 'quenchers', 'quenelles', 'querulous', 'questions', 'quibbling', 'quickness', 'quickened', 'quicklime', 'quicksand', 'quickstep', 'quiescent', 'quietness', 'quietudes', 'quietuses', 'quintuple', 'quipsters', 'quirkiest', 'quislings', 'quitclaim', 'quitrents', 'quittance', 'quivering', 'quizzical', 'quotation', 'quotidian', 'quotients', 'rabbinate', 'racehorse', 'racetrack', 'racketeer', 'raconteur', 'radiation', 'radiances', 'radiating', 'radiative', 'radiators', 'radically', 'radicands', 'radiogram', 'radiology', 'railroads', 'raincoats', 'raindrops', 'rainfalls', 'rainmaker', 'rainstorm', 'rainwater', 'ramifying', 'rampaging', 'rancheros', 'rancidity', 'rancorous', 'randomize', 'ransacked', 'ransoming', 'rapacious', 'rappelled', 'raptorial', 'rapturous', 'rarefying', 'rascality', 'raspberry', 'ratcheted', 'ratifying', 'rationing', 'rationale', 'rattaning', 'raucously', 'ravishing', 'razorback', 'reachable', 'reactions', 'reactance', 'reactants', 'readiness', 'readjusts', 'reaffirms', 'realities', 'realigned', 'realistic', 'realizing', 'reappears', 'rearrange', 'reasoning', 'reassigns', 'reassumed', 'reassumes', 'reassured', 'reassures', 'reawakens', 'rebelling', 'rebellion', 'rebinding', 'rebooting', 'rebounded', 'rebuffing', 'rebutting', 'rebutters', 'rebuttals', 'recalling', 'recanting', 'recapping', 'recapture', 'recasting', 'receiving', 'receivers', 'receptive', 'receptors', 'reception', 'recessing', 'recession', 'recessive', 'recharged', 'recharges', 'rechecked', 'recherche', 'recipient', 'reckoning', 'reclaimed', 'reclining', 'reclusive', 'recognize', 'recoiling', 'recollect', 'recombine', 'recommend', 'recompile', 'recompose', 'reconcile', 'recondite', 'reconfirm', 'reconnect', 'recording', 'recorders', 'recounted', 'recouping', 'recourses', 'recovered', 'recreants', 'recreated', 'recreates', 'recruited', 'recruiter', 'rectangle', 'rectified', 'rectifier', 'rectifies', 'rectitude', 'rectories', 'rectorate', 'rectorial', 'recumbent', 'recurring', 'recurrent', 'recursion', 'recursive', 'recycling', 'redacting', 'redaction', 'redactors', 'redbreast', 'reddening', 'redeeming', 'redeemers', 'redefined', 'redefines', 'redeliver', 'redevelop', 'redheaded', 'redingote', 'redirects', 'redolence', 'redoubles', 'redoubled', 'redounded', 'redressed', 'redresser', 'redresses', 'redstarts', 'reducibly', 'reducible', 'reduction', 'reductive', 'redundant', 'refection', 'refectory', 'referable', 'reference', 'referenda', 'referents', 'referring', 'referrers', 'referrals', 'refilling', 'refinance', 'refitting', 'reflected', 'reflector', 'reflexive', 'reforests', 'reforming', 'reformers', 'reformats', 'refracted', 'refractor', 'refrained', 'refreshed', 'refresher', 'refreshes', 'refueling', 'refulgent', 'refunding', 'refurbish', 'refutably', 'refutable', 'regaining', 'regarding', 'regencies', 'regicides', 'regicidal', 'regiments', 'registers', 'registrar', 'regressed', 'regresses', 'regretful', 'regretted', 'regrouped', 'regularly', 'regulated', 'regulates', 'regulator', 'rehashing', 'rehearing', 'rehearsed', 'rehearses', 'rehearsal', 'rehousing', 'reimburse', 'reinforce', 'reinserts', 'reinstall', 'reinstate', 'reinsured', 'reinsures', 'reinvents', 'reinvests', 'reissuing', 'reiterate', 'rejecting', 'rejecters', 'rejection', 'rejoicing', 'rejoining', 'rejoinder', 'rekindles', 'rekindled', 'relapsing', 'relations', 'relatives', 'relaxants', 'releasing', 'relegated', 'relegates', 'relenting', 'relevance', 'reliances', 'relieving', 'religions', 'religious', 'reliquary', 'relishing', 'relocated', 'relocates', 'reluctant', 'remaining', 'remainder', 'remanding', 'remarking', 'remarried', 'rematches', 'remedying', 'remembers', 'reminding', 'reminders', 'reminisce', 'remission', 'remitting', 'remitters', 'remittals', 'remittent', 'remodeled', 'remounted', 'removably', 'removable', 'renascent', 'rendering', 'rendition', 'renegades', 'renewably', 'renewable', 'renitence', 'renounces', 'renounced', 'renovated', 'renovates', 'renovator', 'renumbers', 'reopening', 'reordered', 'repackage', 'repairing', 'repairmen', 'repairman', 'reparably', 'reparable', 'repartees', 'repayable', 'repayment', 'repealing', 'repeating', 'repeaters', 'repelling', 'repellent', 'repenting', 'repentant', 'repertory', 'rephrased', 'rephrases', 'replacing', 'replanted', 'replaying', 'replenish', 'repletion', 'replicate', 'reporting', 'reporters', 'reportage', 'repossess', 'reprehend', 'represent', 'repressed', 'represses', 'repressor', 'reprieved', 'reprieves', 'reprimand', 'reprinted', 'reprising', 'reprisals', 'reprobate', 'reprocess', 'reproduce', 'reproving', 'reptilian', 'republics', 'republish', 'repudiate', 'repugnant', 'repulsing', 'repulsion', 'repulsive', 'reputably', 'reputable', 'reputedly', 'requester', 'requiring', 'requisite', 'requiting', 'requitals', 'rereading', 'rerouting', 'rerunning', 'rescinded', 'resection', 'reselling', 'resellers', 'resembles', 'resembled', 'resenting', 'resentful', 'reserving', 'reservist', 'reservoir', 'resetting', 'reshipped', 'reshuffle', 'residence', 'residency', 'residents', 'residuals', 'residuary', 'resigning', 'resilient', 'resisting', 'resisters', 'resistive', 'resistors', 'resistant', 'resoluble', 'resolving', 'resonance', 'resonated', 'resonates', 'resonator', 'resorting', 'resounded', 'resources', 'respected', 'respelled', 'respiring', 'responded', 'responder', 'responses', 'restively', 'restarted', 'restating', 'restocked', 'restoring', 'restorers', 'restrains', 'restraint', 'restricts', 'resulting', 'resultant', 'resurgent', 'resurrect', 'resurveys', 'retailing', 'retailers', 'retaining', 'retainers', 'retaliate', 'retarding', 'retardate', 'retelling', 'retention', 'retentive', 'rethought', 'reticence', 'reticules', 'reticular', 'retooling', 'retorting', 'retouched', 'retouches', 'retracing', 'retracted', 'retractor', 'retrained', 'retreaded', 'retreated', 'retrieved', 'retriever', 'retrieves', 'retrieval', 'retrofits', 'returning', 'returnees', 'reuniting', 'revamping', 'revealing', 'reveilles', 'revelries', 'revenants', 'revenging', 'reverence', 'reverends', 'reversing', 'reversion', 'reversals', 'reverting', 'reviewing', 'reviewers', 'revisable', 'revisions', 'revisited', 'revocably', 'revocable', 'revolting', 'revolving', 'revolvers', 'revulsion', 'rewarding', 'rewinding', 'rewording', 'reworking', 'rewriting', 'rewriters', 'rewritten', 'reykjavik', 'rhapsodic', 'rheostats', 'rhetorics', 'rheumatic', 'rhizoidal', 'rhodesian', 'rhomboids', 'rhombuses', 'rhymester', 'ribboning', 'ribosomes', 'ribosomal', 'ricketier', 'rickracks', 'ricochets', 'riddances', 'ridicules', 'ridiculed', 'riffraffs', 'rightists', 'rightness', 'righteous', 'rightmost', 'rigmarole', 'ringsides', 'ringtails', 'ringworms', 'riotously', 'riposting', 'rivalries', 'riverbeds', 'riverboat', 'riverside', 'roadblock', 'roadhouse', 'roadsides', 'roadsters', 'roadstead', 'robberies', 'rochester', 'rockiness', 'rocketing', 'rocketeer', 'roentgens', 'rogations', 'rogueries', 'roistered', 'rollaways', 'rollbacks', 'rollicked', 'romancing', 'romantics', 'rookeries', 'roominess', 'roomettes', 'roommates', 'rootstock', 'roquefort', 'rorschach', 'rosewoods', 'rotarians', 'rotations', 'rottenest', 'rotundity', 'roughness', 'roughened', 'roughhews', 'roughhewn', 'roughneck', 'roughshod', 'roulettes', 'roundness', 'roundelay', 'roundworm', 'routinely', 'rowdiness', 'royalists', 'royalties', 'rubberier', 'rubberize', 'rubbishes', 'rucksacks', 'ruddiness', 'rudiments', 'ruination', 'rumanians', 'rumblings', 'ruminants', 'ruminated', 'ruminates', 'ruminator', 'rummaging', 'rumrunner', 'runabouts', 'rupturing', 'rustiness', 'rusticity', 'rusticate', 'rustproof', 'rutabagas', 'ruthenium', 'sabotaged', 'sabotages', 'saboteurs', 'saccharin', 'sackcloth', 'sacrament', 'sacrifice', 'sacrilege', 'sacristan', 'saddening', 'saddlebag', 'saddlebow', 'sadnesses', 'safeguard', 'safflower', 'sagacious', 'sagebrush', 'sailboats', 'sailcloth', 'sainthood', 'saintlier', 'salaaming', 'salacious', 'salarying', 'salesroom', 'salicylic', 'saliences', 'salivated', 'salivates', 'sallowest', 'saltiness', 'saltboxes', 'saltpeter', 'saltwater', 'salvation', 'salvaging', 'salvagers', 'samaritan', 'samplings', 'sanctions', 'sanctuary', 'sandiness', 'sandaracs', 'sandblast', 'sandboxes', 'sandpaper', 'sandpiper', 'sandstone', 'sandstorm', 'sanitized', 'sanitizer', 'sanitizes', 'sapiences', 'sapodilla', 'sappiness', 'sapphires', 'sapsucker', 'sarcastic', 'sarcomata', 'sardining', 'sardinian', 'sargassos', 'sartorial', 'sashaying', 'sassiness', 'sassafras', 'satanists', 'satellite', 'satiation', 'satiating', 'satieties', 'satinwood', 'satirists', 'satirical', 'satirized', 'satirizes', 'satisfied', 'satisfies', 'satrapies', 'saturable', 'saturated', 'saturates', 'saturator', 'saturdays', 'saturnine', 'sauciness', 'saucepans', 'sauntered', 'sauternes', 'sawhorses', 'saxifrage', 'saxophone', 'scabbiest', 'scabbards', 'scaffolds', 'scalowags', 'scallions', 'scalloped', 'scampered', 'scansions', 'scantiest', 'scantling', 'scapegoat', 'scapulars', 'scariness', 'scarecrow', 'scatology', 'scattered', 'scavenged', 'scavenger', 'scavenges', 'scenarist', 'scenarios', 'schedules', 'scheduled', 'scheduler', 'schematic', 'schistose', 'schizoids', 'schlemiel', 'schlepped', 'schmaltzy', 'schnauzer', 'schnitzel', 'schnozzle', 'scholarly', 'schooling', 'schoolboy', 'schooners', 'schussing', 'sciaticas', 'scientist', 'scientism', 'scimitars', 'scintilla', 'scissions', 'scleroses', 'sclerosis', 'sclerotic', 'scofflaws', 'scorbutic', 'scorching', 'scorecard', 'scorpions', 'scotching', 'scoundrel', 'scourging', 'scrabbles', 'scrabbled', 'scraggier', 'scrambles', 'scrambled', 'scrambler', 'scramming', 'scrapings', 'scrapbook', 'scrapping', 'scrappers', 'scrappily', 'scrappier', 'scratched', 'scratches', 'scrawling', 'scrawnier', 'screaming', 'screamers', 'screeched', 'screeches', 'screening', 'screwiest', 'screwball', 'scribbles', 'scribbled', 'scrimmage', 'scrimping', 'scrimshaw', 'scripting', 'scripture', 'scrivener', 'scrolling', 'scrounged', 'scrounger', 'scrounges', 'scrubbing', 'scrubbers', 'scrubbier', 'scruffier', 'scrunched', 'scrunches', 'scrupling', 'scuffling', 'scullions', 'sculpting', 'sculptors', 'sculpture', 'scurrying', 'scuttling', 'seacoasts', 'seafaring', 'seafarers', 'sealskins', 'seaminess', 'seaplanes', 'searchers', 'searching', 'seascapes', 'seashells', 'seashores', 'seasoning', 'seaworthy', 'sebaceous', 'seborrhea', 'secession', 'secluding', 'seclusion', 'seclusive', 'seconding', 'secondary', 'secrecies', 'secreting', 'secretion', 'secretive', 'secretary', 'secretors', 'secretory', 'sectarian', 'sectioned', 'sectional', 'sectoring', 'sectorial', 'securable', 'sedations', 'sedatives', 'sedentary', 'sediments', 'seditions', 'seditious', 'seducible', 'seduction', 'seductive', 'seediness', 'seedcases', 'seedlings', 'seekingly', 'seemingly', 'seemliest', 'seesawing', 'segmented', 'segmental', 'segregate', 'seigniors', 'selecting', 'selection', 'selective', 'selectors', 'selectmen', 'selectman', 'selfishly', 'semantics', 'semaphore', 'semblance', 'semesters', 'semicolon', 'semifinal', 'semisolid', 'semitones', 'semitonic', 'semivowel', 'semolinas', 'senescing', 'seneschal', 'seniority', 'sensation', 'senseless', 'sensitive', 'sensitize', 'sentences', 'sentenced', 'sentience', 'sentiency', 'sentients', 'sentiment', 'sentinels', 'separable', 'separated', 'separator', 'separates', 'september', 'sepulcher', 'sequences', 'sequenced', 'sequester', 'seraglios', 'serenaded', 'serenades', 'sergeancy', 'sergeants', 'serialize', 'serigraph', 'seriously', 'sermonize', 'serologic', 'serrating', 'serration', 'servicing', 'servility', 'servitors', 'servitude', 'settlings', 'sevenfold', 'seventies', 'seventeen', 'severally', 'severance', 'sewerages', 'sextuples', 'sextupled', 'sextuplet', 'sexuality', 'shabbiest', 'shackling', 'shadiness', 'shadowing', 'shadowbox', 'shagbarks', 'shaggiest', 'shakiness', 'shakedown', 'shallowed', 'shallower', 'shamanism', 'shambling', 'shameless', 'shampooed', 'shamrocks', 'shanghais', 'shantungs', 'shapelier', 'shapeless', 'sharkskin', 'sharpness', 'sharpened', 'shattered', 'sheathing', 'sheepfold', 'sheepskin', 'sheetings', 'sheikdoms', 'shellings', 'shellbark', 'shellfire', 'shellfish', 'sheltered', 'shelvings', 'shepherds', 'shetlands', 'shielding', 'shiftiest', 'shiftless', 'shillings', 'shimmered', 'shimmying', 'shininess', 'shinbones', 'shingling', 'shiningly', 'shinnying', 'shipments', 'shipboard', 'shiploads', 'shipmates', 'shippings', 'shipshape', 'shipwreck', 'shipyards', 'shirtings', 'shirtless', 'shirttail', 'shivering', 'shoddiest', 'shoehorns', 'shoelaces', 'shoemaker', 'shoetrees', 'shootings', 'shoreline', 'shortness', 'shortages', 'shortcake', 'shortcuts', 'shortened', 'shortfall', 'shorthand', 'shorthorn', 'shortstop', 'shoulders', 'shoveling', 'showiness', 'showering', 'showboats', 'showcased', 'showcases', 'showdowns', 'showgirls', 'showpiece', 'showplace', 'shredding', 'shredders', 'shrewdest', 'shrieking', 'shrilling', 'shrillest', 'shrinking', 'shrinkage', 'shriveled', 'shrouding', 'shrubbier', 'shrubbery', 'shrugging', 'shuddered', 'shuffling', 'shunpikes', 'shutdowns', 'shuttered', 'shuttling', 'siberians', 'sibilance', 'sibilants', 'sibylline', 'sicilians', 'sickening', 'sickliest', 'sickrooms', 'sideboard', 'sideburns', 'sidekicks', 'sidelight', 'sidelined', 'sidelines', 'sidesteps', 'sideswipe', 'sidetrack', 'sidewalks', 'sightings', 'sightless', 'sightseer', 'signaling', 'signalers', 'signalize', 'signatory', 'signature', 'signboard', 'signeting', 'signified', 'signifier', 'signifies', 'signposts', 'silencing', 'silencers', 'silicates', 'siliceous', 'silicones', 'silicoses', 'silicosis', 'silkiness', 'silkworms', 'silliness', 'silvering', 'similarly', 'simmering', 'simonized', 'simonizes', 'simpering', 'simpatico', 'simpleton', 'simplexes', 'simulacra', 'simulated', 'simulates', 'simulator', 'simulcast', 'sincerity', 'sincerest', 'sincerely', 'sinecures', 'singapore', 'singleton', 'singsongs', 'singulars', 'sinistral', 'sinkholes', 'sinuosity', 'sinuously', 'sinusitis', 'siphoning', 'sissified', 'situating', 'situation', 'sixteenth', 'sixtieths', 'skeletons', 'skeptical', 'sketching', 'sketchily', 'sketchier', 'skewering', 'skewbalds', 'skidooing', 'skimpiest', 'skinflint', 'skinniest', 'skintight', 'skittered', 'skullcaps', 'skydiving', 'skydivers', 'skyjacked', 'skyjacker', 'skylarked', 'skylights', 'skyrocket', 'skywriter', 'slackness', 'slackened', 'slandered', 'slanderer', 'slangiest', 'slantwise', 'slaphappy', 'slapstick', 'slathered', 'slatterns', 'slaughter', 'slavering', 'slaveries', 'slavishly', 'sleaziest', 'sleddings', 'sleepiest', 'sleepless', 'sleepwalk', 'sleighing', 'slenderer', 'sleuthing', 'sliceable', 'slighting', 'slightest', 'sliminess', 'slingshot', 'shinkiest', 'slipcased', 'slipcases', 'slipcover', 'slipknots', 'slipovers', 'slippages', 'slithered', 'slivering', 'slivovitz', 'slobbered', 'sloppiest', 'slouching', 'sloughing', 'slowdowns', 'slowpokes', 'sludgiest', 'slugabeds', 'slugfests', 'sluggards', 'slumbered', 'slumlords', 'slushiest', 'smallness', 'smalltime', 'smartened', 'smattered', 'smeariest', 'smelliest', 'smilingly', 'smirching', 'smockings', 'smoggiest', 'smokiness', 'smokeless', 'smoldered', 'smooching', 'smoothing', 'smoothest', 'smoothens', 'smothered', 'smudgiest', 'smuggling', 'smugglers', 'smuttiest', 'snaffling', 'snakebite', 'snakeroot', 'snakeskin', 'snappiest', 'snapshots', 'snatching', 'snatchers', 'snazziest', 'sneakiest', 'snickered', 'sniffling', 'sniggered', 'snippiest', 'snitching', 'snitchers', 'sniveling', 'snobbisms', 'snoopiest', 'snootiest', 'snorkeled', 'snowiness', 'snowballs', 'snowbirds', 'snowbound', 'snowdrift', 'snowdrops', 'snowfalls', 'snowflake', 'snowplows', 'snowshoes', 'snowstorm', 'snowsuits', 'snuffling', 'snuggling', 'soapiness', 'soapboxes', 'soapstone', 'soapworts', 'sobbingly', 'soberness', 'sobriquet', 'socialism', 'socialist', 'socialite', 'socialize', 'societies', 'sociology', 'soddening', 'softballs', 'softening', 'softeners', 'softwoods', 'sogginess', 'sojourned', 'sojourner', 'solariums', 'soldering', 'soldiered', 'soldierly', 'solecisms', 'solemnity', 'solemnest', 'solemnize', 'solenoids', 'solicited', 'solicitor', 'soliloquy', 'solipsism', 'solipsist', 'solitaire', 'solitudes', 'solstices', 'solutions', 'sombreros', 'someplace', 'something', 'sometimes', 'somewhere', 'sommelier', 'somnolent', 'sonargram', 'songbirds', 'songfests', 'songsters', 'sonneteer', 'sootiness', 'sophistic', 'sophistry', 'sophomore', 'soporific', 'sorcerers', 'sorceries', 'sorceress', 'soreheads', 'sorrowing', 'sorrowful', 'soubrette', 'soulfully', 'soundness', 'soundings', 'soundless', 'soupiness', 'soupspoon', 'sourdough', 'southeast', 'southerly', 'southpaws', 'southward', 'southwest', 'souvenirs', 'sovereign', 'spacebars', 'spaceport', 'spaceship', 'spackling', 'spadework', 'spaghetti', 'spangling', 'spaniards', 'spareribs', 'sparingly', 'sparkling', 'sparklers', 'spasmodic', 'spatially', 'spattered', 'speakeasy', 'spearhead', 'spearmint', 'specially', 'specialty', 'specified', 'specifies', 'specifics', 'specimens', 'speckling', 'spectacle', 'spectator', 'spectrums', 'speculate', 'speediest', 'speedboat', 'speedster', 'speedways', 'speedwell', 'spellings', 'spellbind', 'spelunker', 'spermatic', 'sphagnums', 'sphenoids', 'spherical', 'spheroids', 'sphincter', 'spiciness', 'spiffiest', 'spikiness', 'spikenard', 'spillages', 'spillways', 'spininess', 'spinaches', 'spindling', 'spindlier', 'spineless', 'spinnaker', 'spinneret', 'spinsters', 'spiracles', 'spiraling', 'spirillum', 'spiriting', 'spiritual', 'spirogyra', 'spitfires', 'spittoons', 'splashing', 'splashily', 'splashier', 'splatters', 'splayfeet', 'splayfoot', 'splendors', 'splinting', 'splinters', 'splintery', 'splitting', 'splotched', 'splotches', 'splurging', 'splutters', 'spoilages', 'spokesmen', 'spokesman', 'spoliator', 'spongiest', 'sponsored', 'spookiest', 'spoonbill', 'spoonfuls', 'sporangia', 'sportiest', 'sportsmen', 'sportsman', 'spotlight', 'spottiest', 'spraining', 'sprawling', 'spreading', 'spreaders', 'sprigging', 'sprightly', 'springing', 'springier', 'springbok', 'sprinkles', 'sprinkled', 'sprinkler', 'sprinting', 'sprinters', 'sprockets', 'sprouting', 'spunkiest', 'sputtered', 'squabbles', 'squabbled', 'squadrons', 'squalling', 'squanders', 'squashing', 'squashier', 'squatting', 'squatters', 'squattest', 'squawking', 'squeaking', 'squeakier', 'squealing', 'squealers', 'squeamish', 'squeegees', 'squeezing', 'squelched', 'squelches', 'squiggles', 'squiggled', 'squinting', 'squirming', 'squirmier', 'squirrels', 'squirting', 'squishing', 'squishier', 'stability', 'stabilize', 'staccatos', 'staginess', 'stagehand', 'staggered', 'stagnancy', 'stagnated', 'stagnates', 'stainless', 'staircase', 'stairways', 'stairwell', 'stalemate', 'stalinist', 'stallions', 'stalwarts', 'stammered', 'stampeded', 'stampedes', 'stanching', 'stanchion', 'standings', 'standards', 'standoffs', 'standouts', 'standpipe', 'starboard', 'starching', 'starchier', 'stargazed', 'stargazer', 'stargazes', 'starlings', 'starlight', 'starriest', 'startling', 'statement', 'statehood', 'statelier', 'stateless', 'stateroom', 'stateside', 'statesmen', 'statesman', 'statewide', 'stationed', 'stationer', 'statistic', 'statuette', 'statutory', 'stauncher', 'staunchly', 'steadiest', 'steadying', 'steadfast', 'steamiest', 'steamboat', 'steamship', 'steatites', 'steeliest', 'steelwork', 'steelyard', 'steenboks', 'steepened', 'steerages', 'steersmen', 'steersman', 'stegosaur', 'stenciled', 'stenotype', 'stepchild', 'sterility', 'sterilize', 'sterlings', 'stevedore', 'stewarded', 'stibnites', 'stickiest', 'stickball', 'stickling', 'sticklers', 'stickpins', 'stiffness', 'stiffened', 'stiffener', 'stigmatic', 'stilettos', 'stillborn', 'stillness', 'stimulant', 'stimulate', 'stingiest', 'stingrays', 'stinkpots', 'stinkweed', 'stippling', 'stipulate', 'stirrings', 'stitching', 'stockiest', 'stockings', 'stockaded', 'stockades', 'stockholm', 'stockinet', 'stockpile', 'stockroom', 'stockyard', 'stodgiest', 'stoicisms', 'stoically', 'stolidity', 'stomached', 'stomacher', 'stonewall', 'stoneware', 'stonework', 'stopcocks', 'stoplight', 'stopovers', 'stoppages', 'stopwatch', 'storeroom', 'stormiest', 'storybook', 'stovepipe', 'stowaways', 'straddles', 'straddled', 'straggles', 'straggled', 'straggler', 'straining', 'strainers', 'straitens', 'stranding', 'strangers', 'strangest', 'strangles', 'strangely', 'strangled', 'strangler', 'strapping', 'stratagem', 'strategic', 'streaking', 'streakily', 'streakier', 'streaming', 'streamers', 'streetcar', 'strengths', 'strenuous', 'stressing', 'stressful', 'stretched', 'stretcher', 'stretches', 'streusels', 'striating', 'striation', 'strictest', 'stricture', 'stridence', 'stridency', 'strikeout', 'stringing', 'stringers', 'stringier', 'stringent', 'stripling', 'stripping', 'strippers', 'strivings', 'strolling', 'strollers', 'strongest', 'strongbox', 'strontium', 'stropping', 'structure', 'struggles', 'struggled', 'struggler', 'strumming', 'strummers', 'strumpets', 'strutting', 'stubbiest', 'studbooks', 'stuffiest', 'stuffings', 'stumbling', 'stupefied', 'stupefies', 'stupidity', 'stupidest', 'stuporous', 'sturdiest', 'sturgeons', 'stuttered', 'stylistic', 'stylizing', 'stymieing', 'styrofoam', 'suavities', 'subaltern', 'subatomic', 'subdivide', 'subfamily', 'subgenera', 'subgroups', 'subjacent', 'subjected', 'subjoined', 'subjugate', 'subleased', 'subleases', 'subliming', 'sublimity', 'sublimest', 'sublimate', 'submarine', 'submerged', 'submerges', 'submersed', 'submerses', 'submitted', 'submittal', 'subnormal', 'suborders', 'suborning', 'subphylum', 'subpoenas', 'subscribe', 'subscript', 'subsiding', 'subsidies', 'subsidize', 'subsisted', 'substance', 'substrate', 'substrata', 'subsuming', 'subsystem', 'subtenant', 'subtended', 'subtitles', 'subtitled', 'subtotals', 'subtracts', 'suburbias', 'subverted', 'succeeded', 'successes', 'successor', 'succoring', 'succotash', 'succulent', 'succumbed', 'suckering', 'suctioned', 'sufferers', 'suffering', 'sufficing', 'suffixing', 'suffocate', 'suffrages', 'suffusing', 'suffusion', 'sugarcoat', 'sugarless', 'sugarplum', 'suggested', 'suitcases', 'sukiyakis', 'sulfurous', 'sullenest', 'sultanate', 'sultriest', 'summation', 'summarily', 'summaries', 'summarize', 'summering', 'summoning', 'summonses', 'sumptuary', 'sumptuous', 'sunbathed', 'sunbather', 'sunbathes', 'sunbonnet', 'sunburned', 'sunbursts', 'sundering', 'sunflower', 'sunlights', 'sunshades', 'sunstroke', 'sunstruck', 'suntanned', 'superegos', 'superfine', 'superheat', 'superhigh', 'superiors', 'supernova', 'superpose', 'supersede', 'superstar', 'supervene', 'supervise', 'supplants', 'suppliant', 'suppliers', 'supplying', 'supported', 'supporter', 'supposing', 'suppurate', 'supremacy', 'supremely', 'surceases', 'surcharge', 'surcingle', 'surfacing', 'surfboard', 'surfeited', 'surgeries', 'surliness', 'surmising', 'surmounts', 'surnaming', 'surpassed', 'surpasses', 'surplices', 'surpluses', 'surprints', 'surprised', 'surprises', 'surrender', 'surrogate', 'surrounds', 'surveying', 'surveyors', 'surviving', 'survivors', 'survivals', 'suspected', 'suspended', 'suspicion', 'sustained', 'suzerains', 'swaddling', 'swaggered', 'swallowed', 'swampiest', 'swampland', 'swankiest', 'swappings', 'swarthily', 'swarthier', 'swastikas', 'swaybacks', 'swaziland', 'swearword', 'sweatiest', 'sweatband', 'sweatshop', 'sweepings', 'sweetness', 'sweetened', 'sweetener', 'sweetmeat', 'swellings', 'sweltered', 'sweptback', 'swiftness', 'swimmeret', 'swimsuits', 'swindling', 'swindlers', 'swineherd', 'switching', 'switchmen', 'switchman', 'swiveling', 'swizzling', 'swordfish', 'swordplay', 'swordsmen', 'swordsman', 'swordtail', 'sybarites', 'sybaritic', 'sycamores', 'sycophant', 'syllabics', 'syllabify', 'syllables', 'syllabubs', 'syllogism', 'symbioses', 'symbiosis', 'symbiotic', 'symboling', 'symbolist', 'symbolism', 'symbolize', 'symbology', 'symmetric', 'symphonic', 'symposium', 'synagogue', 'synclines', 'synclinal', 'syncopate', 'syndicate', 'syndromes', 'synergies', 'synergism', 'syntactic', 'syntheses', 'synthesis', 'synthetic', 'tablehops', 'tableland', 'tabletops', 'tableware', 'tabulated', 'tabulates', 'tabulator', 'tackiness', 'tacklings', 'taconites', 'tactfully', 'tactician', 'tactility', 'tahitians', 'tailbacks', 'tailboard', 'tailgated', 'tailgates', 'taillight', 'tailoring', 'tailpiece', 'tailpipes', 'tailraces', 'tailspins', 'tailwinds', 'takedowns', 'takeovers', 'talismans', 'talkative', 'tallyings', 'tamaracks', 'tamarinds', 'tamarisks', 'tampering', 'tangerine', 'tangibles', 'tanneries', 'tantalite', 'tantalize', 'tapeworms', 'tarantula', 'tardiness', 'targeting', 'tarlatans', 'tarnished', 'tarnishes', 'tarpapers', 'tarpaulin', 'tarragons', 'tartarous', 'tasseling', 'tastiness', 'tasteless', 'tattering', 'tattooing', 'tautology', 'tawdriest', 'taxations', 'taxidermy', 'taximeter', 'taxonomic', 'taxpaying', 'taxpayers', 'teachably', 'teachable', 'teachings', 'teacupful', 'teahouses', 'teakettle', 'teammates', 'teamsters', 'teardrops', 'tearfully', 'teasingly', 'teaspoons', 'technical', 'technique', 'tediously', 'teetering', 'teguments', 'telecasts', 'telegrams', 'telegraph', 'telemeter', 'telemetry', 'teleology', 'telepathy', 'telephone', 'telephony', 'telephoto', 'teleplays', 'telescope', 'telethons', 'teletyped', 'teletypes', 'televised', 'televises', 'tellingly', 'telltales', 'telluride', 'tellurium', 'telophase', 'tempering', 'temperate', 'templates', 'temporals', 'temporary', 'temporize', 'temptress', 'tenacious', 'tenancies', 'tenanting', 'tendering', 'tenderize', 'tenements', 'tenebrous', 'tennessee', 'tenseness', 'tensility', 'tentacles', 'tentative', 'tentmaker', 'tenuously', 'teriyakis', 'termagant', 'terminals', 'terminate', 'terracing', 'terrapins', 'terrarium', 'terrazzos', 'terrified', 'terrifies', 'territory', 'terrorist', 'terrorism', 'terrorize', 'terseness', 'testiness', 'testament', 'testacies', 'testators', 'testatrix', 'testicles', 'testified', 'testifier', 'testifies', 'testimony', 'tethering', 'tetrarchs', 'tetrarchy', 'textbooks', 'thalassic', 'thankless', 'thatching', 'theatrics', 'theocracy', 'theocrats', 'theorists', 'theorized', 'theorizer', 'theorizes', 'theosophy', 'therapies', 'therapist', 'therefore', 'therefrom', 'thereunto', 'thereupon', 'therewith', 'thermions', 'thermally', 'thermoses', 'thesaurus', 'thespians', 'thiamines', 'thickened', 'thickener', 'thickness', 'thighbone', 'thinkably', 'thinkable', 'thirsting', 'thirstily', 'thirstier', 'thirteens', 'thirtieth', 'thitherto', 'tholepins', 'thorniest', 'thousands', 'thralldom', 'thrashing', 'thrashers', 'threading', 'threadier', 'threatens', 'threefold', 'threesome', 'threshing', 'threshers', 'threshold', 'thriftily', 'thriftier', 'thrilling', 'thrillers', 'throatier', 'throbbing', 'thrombins', 'thronging', 'throttles', 'throttled', 'throwaway', 'throwback', 'thrumming', 'thrusting', 'thumbhole', 'thumbnail', 'thumbnuts', 'thumbtack', 'thundered', 'thuribles', 'thursdays', 'thwacking', 'thwarting', 'thyroxins', 'ticketing', 'tidelands', 'tidemarks', 'tidewater', 'tightness', 'tightened', 'tightrope', 'tightwads', 'tigresses', 'timbering', 'timecards', 'timeliest', 'timelines', 'timepiece', 'timetable', 'timothies', 'timpanist', 'tinctured', 'tinctures', 'tinderbox', 'tinkering', 'tinseling', 'tinsmiths', 'tintyping', 'tipsiness', 'tiptoeing', 'tiredness', 'titillate', 'titration', 'tittering', 'toadstool', 'toboggans', 'toilettes', 'tokenisms', 'tolerably', 'tolerable', 'tolerance', 'tolerated', 'tolerates', 'tolerator', 'tollbooth', 'tollgates', 'tomahawks', 'tombstone', 'tomorrows', 'tonsorial', 'tonsuring', 'toolboxes', 'toothiest', 'toothache', 'toothless', 'toothpick', 'toothsome', 'topflight', 'topiaries', 'topologic', 'topsiders', 'topstitch', 'toreadors', 'tormented', 'tormentor', 'tornadoes', 'torpedoed', 'torpedoes', 'tortillas', 'tortoises', 'torturing', 'torturers', 'torturous', 'totalizer', 'tottering', 'touchiest', 'touchable', 'touchback', 'touchdown', 'toughness', 'toughened', 'tournedos', 'towelings', 'towheaded', 'townsfolk', 'townships', 'toxicants', 'traceably', 'traceable', 'traceries', 'trachomas', 'trackages', 'trackless', 'tractions', 'tractably', 'tractable', 'trademark', 'tradeoffs', 'tradesmen', 'tradesman', 'tradition', 'traducing', 'traducers', 'tragedies', 'tragedian', 'trainlord', 'traipsing', 'trammeled', 'trampling', 'transacts', 'transcend', 'transepts', 'transfers', 'transform', 'transfuse', 'transient', 'transited', 'translate', 'transmits', 'transmute', 'transonic', 'transpire', 'transport', 'transpose', 'transship', 'trapezium', 'trapezoid', 'trappings', 'trashiest', 'traumatic', 'travailed', 'traveling', 'travelers', 'traversed', 'traverses', 'traversal', 'treachery', 'treadling', 'treadmill', 'treasured', 'treasurer', 'treasures', 'treatment', 'treatises', 'trellises', 'trembling', 'tremulous', 'trenching', 'trenchant', 'trendiest', 'trepanned', 'trephined', 'trephines', 'triangles', 'tribesmen', 'tribesman', 'tribunals', 'tributary', 'trichinae', 'trickiest', 'trickling', 'trickster', 'triclinic', 'tricolors', 'tricuspid', 'tricycles', 'triennial', 'trifocals', 'triggered', 'trillions', 'trilliums', 'trilobite', 'trilogies', 'trimester', 'trimmings', 'trinities', 'trinomial', 'trioxides', 'triplexes', 'triptychs', 'trisected', 'trisector', 'triumphed', 'triumphal', 'triumvirs', 'trivalent', 'triweekly', 'trombones', 'troopship', 'troubling', 'trouncing', 'trousseau', 'troweling', 'truancies', 'truckages', 'truckling', 'truckload', 'truculent', 'trueloves', 'trumpeted', 'trumpeter', 'truncated', 'truncates', 'truncheon', 'trundling', 'trustiest', 'tubercles', 'tuberoses', 'tuckering', 'tularemia', 'tumefying', 'tumescent', 'tungstens', 'tunisians', 'tunneling', 'turbojets', 'turboprop', 'turbulent', 'turgidity', 'turnabout', 'turncoats', 'turnovers', 'turnpikes', 'turnstile', 'turntable', 'turpitude', 'turquoise', 'tutelages', 'tutorials', 'twaddling', 'tweediest', 'twentieth', 'twiddling', 'twilights', 'twinkling', 'twistable', 'twitching', 'twittered', 'tympanist', 'typecasts', 'typefaces', 'typewrite', 'typewrote', 'typically', 'typifying', 'tyrannies', 'tyrannous', 'tyrannize', 'tyrosines', 'ukrainian', 'ulcerated', 'ulcerates', 'ultimates', 'ultimatum', 'ultrahigh', 'ululating', 'ululation', 'umbilical', 'umbilicus', 'umbraging', 'unbrellas', 'umpteenth', 'unabashed', 'unadorned', 'unadvised', 'unaligned', 'unalloyed', 'unaltered', 'unanimity', 'unanimous', 'unbalance', 'unbarring', 'unbeliefs', 'unbending', 'unbinding', 'unblended', 'unbolting', 'unbosomed', 'unbounded', 'unbridled', 'unbuckles', 'unbuckled', 'unburdens', 'unbuttons', 'uncannily', 'uncapping', 'unceasing', 'uncenters', 'uncertain', 'unchained', 'unchanged', 'uncharged', 'uncharted', 'unchecked', 'unclaimed', 'unclasped', 'uncleanly', 'uncloaked', 'unclosing', 'unclothed', 'unclothes', 'unclouded', 'uncoiling', 'unconcern', 'uncorking', 'uncounted', 'uncouples', 'uncoupled', 'uncovered', 'uncrossed', 'uncrosses', 'undamaged', 'undaunted', 'undecided', 'undefiled', 'undefined', 'underacts', 'underarms', 'underbids', 'undercoat', 'undercuts', 'underdogs', 'underdone', 'underfeed', 'underfoot', 'undergird', 'undergirt', 'undergoes', 'undergone', 'underhand', 'underling', 'underlain', 'underlies', 'underline', 'undermine', 'undermost', 'underpaid', 'underpass', 'underpays', 'underpins', 'underplay', 'underrate', 'undersell', 'undershot', 'underside', 'undersign', 'undersize', 'undersold', 'undertake', 'undertone', 'undertook', 'undertows', 'underwear', 'underwent', 'undesired', 'undiluted', 'undivided', 'undoubted', 'undreamed', 'undressed', 'undresses', 'undulated', 'undulates', 'undutiful', 'unearthed', 'unearthly', 'unequaled', 'unequally', 'unethical', 'unexposed', 'unfailing', 'unfastens', 'unfeeling', 'unfeigned', 'unfitness', 'unfitting', 'unfledged', 'unfolding', 'unfounded', 'unfrocked', 'unfrosted', 'unfurling', 'unguarded', 'ungulates', 'unhanding', 'unhappily', 'unharness', 'unhealthy', 'unheeding', 'unhelpful', 'unhinging', 'unhitched', 'unhitches', 'unhooking', 'unhorsing', 'unhurried', 'unicycles', 'uniformed', 'uniformly', 'unimpeded', 'uninjured', 'uninsured', 'uninvited', 'unionists', 'unionisms', 'unionized', 'unionizes', 'unisexual', 'unitarian', 'univalent', 'univalves', 'universes', 'universal', 'unkindest', 'unknowing', 'unlatched', 'unlatches', 'unlearned', 'unleashed', 'unleashes', 'unlimbers', 'unlimited', 'unloading', 'unlocking', 'unloosing', 'unloosens', 'unluckily', 'unluckier', 'unmanlier', 'unmanning', 'unmarried', 'unmasking', 'unmatched', 'unmindful', 'unmusical', 'unnamable', 'unnatural', 'unnerving', 'unnoticed', 'unopposed', 'unpacking', 'unpainted', 'unpinning', 'unpitying', 'unplanned', 'unplugged', 'unpopular', 'unraveled', 'unreadier', 'unreality', 'unreeling', 'unrefined', 'unrelated', 'unrivaled', 'unrolling', 'unruffled', 'unsaddles', 'unsaddled', 'unscathed', 'unscented', 'unscrewed', 'unsealing', 'unseating', 'unselfish', 'unsettles', 'unsettled', 'unshackle', 'unsheathe', 'unsightly', 'unskilled', 'unsmiling', 'unsnapped', 'unsnarled', 'unsparing', 'unspoiled', 'unspotted', 'unstained', 'unstirred', 'unstopped', 'unstrings', 'unstudied', 'unsullied', 'untainted', 'untangles', 'untangled', 'untenably', 'untenable', 'untouched', 'untrained', 'untreated', 'untutored', 'untwining', 'untwisted', 'untypical', 'unusually', 'unuttered', 'unvarying', 'unveiling', 'unwelcome', 'unwilling', 'unwinding', 'unwitting', 'unworldly', 'unwrapped', 'unwritten', 'unzipping', 'upanishad', 'upbraided', 'upcountry', 'upgrading', 'upheaving', 'upheavals', 'upholding', 'upholster', 'uplifting', 'uppercase', 'uppercuts', 'uppermost', 'upraising', 'uprisings', 'uprooting', 'upsetting', 'upstaging', 'upstrokes', 'upsurging', 'upturning', 'urbanites', 'urbanized', 'urbanizes', 'urgencies', 'urination', 'urinaries', 'urinating', 'urologist', 'uruguayan', 'usability', 'uselessly', 'usualness', 'usufructs', 'utilities', 'utilizing', 'utterance', 'uttermost', 'vacancies', 'vacations', 'vaccinate', 'vacillate', 'vacuities', 'vacuuming', 'vagabonds', 'vagueness', 'vainglory', 'valancing', 'valencies', 'valentine', 'valerians', 'valiantly', 'validated', 'validates', 'valkyries', 'valuation', 'valuables', 'valuators', 'valueless', 'vamoosing', 'vampirism', 'vandalism', 'vandalize', 'vanguards', 'vanillins', 'vanishing', 'vaporized', 'vaporizer', 'vaporizes', 'variation', 'variances', 'variables', 'variegate', 'varieties', 'variously', 'varnished', 'varnishes', 'varsities', 'vasectomy', 'vasomotor', 'vassalage', 'vaultings', 'vectoring', 'vectorial', 'vegetable', 'vegetated', 'vegetates', 'vehemence', 'vehicular', 'velvetier', 'velveteen', 'venations', 'vendettas', 'vendibles', 'veneering', 'venerably', 'venerable', 'venerated', 'venerates', 'venerator', 'venezuela', 'vengeance', 'veniality', 'ventilate', 'ventricle', 'venturing', 'venturous', 'veracious', 'verandahs', 'verbalist', 'verbalism', 'verbalize', 'verbiages', 'verbosity', 'verdigris', 'verifying', 'verifiers', 'veritably', 'veritable', 'vermicide', 'vermiform', 'vermifuge', 'vermilion', 'verminous', 'vermouths', 'veronicas', 'versatile', 'versicles', 'versified', 'versifier', 'versifies', 'vertebrae', 'vertebral', 'verticals', 'vertigoes', 'vesicants', 'vesicated', 'vesicates', 'vesicular', 'vestments', 'vestibule', 'vestigial', 'vestrymen', 'vestryman', 'vesturing', 'vexations', 'vexatious', 'viability', 'vibrating', 'vibration', 'vibrators', 'vibratory', 'viburnums', 'vicarages', 'vicarious', 'viceregal', 'vicinages', 'viciously', 'victimize', 'victories', 'victorias', 'victorian', 'videlicet', 'videotape', 'viewpoint', 'vigesimal', 'vigilance', 'vigilante', 'vignetted', 'vignettes', 'vilifying', 'villagers', 'vindicate', 'vineyards', 'vintaging', 'violation', 'violating', 'violative', 'violators', 'violences', 'violently', 'violinist', 'virginity', 'virginals', 'virtually', 'virtuosic', 'virtuosos', 'virulence', 'viscidity', 'viscosity', 'viscounts', 'visigoths', 'visioning', 'visionary', 'visitants', 'visualize', 'vitalized', 'vitalizes', 'vitiating', 'vitiation', 'vitiators', 'vitrified', 'vitrifies', 'vitrioled', 'vitriolic', 'vivacious', 'vividness', 'vivifying', 'vivisects', 'vocalists', 'vocalized', 'vocalizes', 'vocations', 'vocatives', 'voiceless', 'volcanoes', 'volitions', 'volleying', 'voltmeter', 'voluntary', 'volunteer', 'voodooing', 'voodooism', 'voracious', 'vouchsafe', 'voyageurs', 'voyeurism', 'vulcanite', 'vulcanize', 'vulgarism', 'vulgarity', 'vulgarian', 'vulgarize', 'wackiness', 'waggishly', 'wagnerian', 'wagonette', 'wainscots', 'waistband', 'waistcoat', 'waistline', 'walkaways', 'wallabies', 'wallboard', 'walloping', 'wallowing', 'wallpaper', 'wanderers', 'wandering', 'wantoning', 'wardrobes', 'wardrooms', 'wardships', 'warehouse', 'warmonger', 'warningly', 'warplanes', 'warranted', 'warrantor', 'washables', 'washboard', 'washbowls', 'washcloth', 'washrooms', 'washstand', 'waspishly', 'wassailed', 'wasteland', 'watchdogs', 'watchword', 'wateriest', 'waterfall', 'waterfowl', 'watergate', 'waterless', 'waterloos', 'watermark', 'watershed', 'waterside', 'waterways', 'wavebands', 'wayfaring', 'wayfarers', 'waylaying', 'weakening', 'weakliest', 'weaklings', 'wealthily', 'wealthier', 'weaponing', 'weariness', 'wearables', 'wearisome', 'weaseling', 'weathered', 'wednesday', 'weekended', 'weighting', 'weightier', 'weirdness', 'welcoming', 'weltering', 'westbound', 'westerner', 'westwards', 'whaleboat', 'whalebone', 'wharfages', 'wheedling', 'wheeziest', 'whereases', 'wherefore', 'wherefrom', 'whereupon', 'wherewith', 'whetstone', 'whichever', 'whimpered', 'whimsical', 'whininess', 'whinnying', 'whipcords', 'whippings', 'whipsawed', 'whirligig', 'whirlpool', 'whirlwind', 'whispered', 'whistling', 'whistlers', 'whiteness', 'whitecaps', 'whitefish', 'whitening', 'whiteners', 'whiteouts', 'whitewall', 'whitewash', 'whittling', 'whodunits', 'wholeness', 'wholesale', 'wholesome', 'whooshing', 'whosoever', 'wickedest', 'wieldable', 'wigwagged', 'wildfires', 'willfully', 'willingly', 'willowier', 'windiness', 'windblown', 'windbreak', 'windfalls', 'windmills', 'windowing', 'windpipes', 'windrowed', 'windsocks', 'windstorm', 'windswept', 'windwards', 'wineglass', 'wineskins', 'wingdings', 'wingspans', 'winnowing', 'wintering', 'winterize', 'wintriest', 'wisconsin', 'wiseacres', 'wisecrack', 'wishbones', 'wispiness', 'wisterias', 'wistfully', 'withering', 'withdraws', 'withdrawn', 'withholds', 'withstand', 'withstood', 'witnessed', 'witnesses', 'wittiness', 'witticism', 'wittingly', 'wobbliest', 'woebegone', 'wolfhound', 'wolverine', 'womanhood', 'womanized', 'womanizer', 'womanizes', 'womankind', 'womanlier', 'womenfolk', 'wonderful', 'wondering', 'woodiness', 'woodbines', 'woodchuck', 'woodcraft', 'woodlands', 'woodpiles', 'woodsheds', 'woodwinds', 'woolliest', 'wooziness', 'wordiness', 'wordbooks', 'workbench', 'workbooks', 'workboxes', 'workhands', 'workhorse', 'workhouse', 'workloads', 'workrooms', 'worksheet', 'workshops', 'workspace', 'worktable', 'workweeks', 'worldlier', 'worldwide', 'wormholes', 'wormwoods', 'worriedly', 'worrisome', 'worrywart', 'worsening', 'worshiped', 'worshiper', 'worstings', 'worthiest', 'worthless', 'wrangling', 'wranglers', 'wrappings', 'wreathing', 'wreckages', 'wrenching', 'wrestling', 'wrestlers', 'wriggling', 'wrinkling', 'wristband', 'wrongdoer', 'xenophobe', 'xylophone', 'yachtsmen', 'yachtsman', 'yardstick', 'yarmulkes', 'yearbooks', 'yearlings', 'yearnings', 'yellowing', 'yesterday', 'yorkshire', 'youngster', 'ytterbium', 'yugoslavs', 'zealously', 'zeppelins', 'ziggurats', 'zigzagged', 'zinfandel', 'zippiness', 'zippering', 'zirconium', 'zitherist', 'zoography', 'zoologies', 'zoologist', 'zoophytes', 'zoospores', 'zwiebacks', 'zygomatic', 'zygospore', 'zymogenic', 'zymurgies', 'abandoners', 'abatements', 'abbotships', 'abbreviate', 'abdicating', 'abdication', 'abductions', 'aberration', 'abhorrence', 'abjectness', 'abnegating', 'abnegation', 'abnegators', 'abnormally', 'abolishing', 'abominable', 'abominably', 'abominated', 'abominates', 'abominator', 'aboriginal', 'aborigines', 'abortional', 'abortively', 'abrasively', 'abridgment', 'abrogating', 'abrogation', 'abrogators', 'abruptness', 'abscessing', 'absconders', 'absconding', 'absolutely', 'absolution', 'absolutism', 'absolutist', 'absorbency', 'absorption', 'abstainers', 'abstaining', 'abstemious', 'abstention', 'abstinence', 'abstracted', 'abstracter', 'abstractly', 'abstrusely', 'absurdness', 'abundantly', 'academical', 'accelerate', 'accentuate', 'acceptable', 'acceptably', 'acceptance', 'accessible', 'accessibly', 'accessions', 'accidental', 'acclaimers', 'acclaiming', 'acclimated', 'acclimates', 'accomplice', 'accomplish', 'accordance', 'accordions', 'accostable', 'accountant', 'accounting', 'accredited', 'accretions', 'accumulate', 'accurately', 'accursedly', 'accusation', 'accusative', 'accusatory', 'accusingly', 'accustomed', 'acerbating', 'acetifying', 'achievable', 'achromatic', 'acidifiers', 'acidifying', 'acidulated', 'acidulates', 'acoustical', 'acquainted', 'acquiesced', 'acquiesces', 'acquirable', 'acquittals', 'acquitting', 'acrobatics', 'actionable', 'activating', 'activation', 'activators', 'activeness', 'activities', 'actualized', 'actualizes', 'adaptation', 'adaptively', 'addictions', 'additional', 'addressees', 'addressing', 'adequately', 'adhesional', 'adhesively', 'adjacently', 'adjectival', 'adjectives', 'adjourning', 'adjudicate', 'adjuratory', 'adjustable', 'adjustment', 'administer', 'admiration', 'admiringly', 'admissible', 'admissibly', 'admissions', 'admittance', 'admittedly', 'admixtures', 'admonished', 'admonishes', 'admonition', 'adolescent', 'adoptively', 'adornments', 'adrenaline', 'adroitness', 'adsorbable', 'adsorbents', 'adsorption', 'adsorptive', 'adulterant', 'adulterate', 'adulterers', 'adulteress', 'adulteries', 'adulterous', 'advantaged', 'advantages', 'adventured', 'adventurer', 'adventures', 'advertised', 'advertiser', 'advertises', 'advisement', 'advisories', 'advocacies', 'advocating', 'aerialists', 'aerobatics', 'aerologist', 'aeronautic', 'aesthetics', 'affability', 'affectedly', 'affections', 'affidavits', 'affiliated', 'affiliates', 'affinities', 'affirmable', 'afflicting', 'affliction', 'afflictive', 'affluently', 'affordable', 'affronting', 'aficionado', 'afterbirth', 'afterimage', 'aftermaths', 'afternoons', 'aftershave', 'aftertaste', 'aggrandize', 'aggravated', 'aggravates', 'aggregated', 'aggregates', 'aggression', 'aggressive', 'aggressors', 'aggrieving', 'agitatedly', 'agitations', 'agreements', 'agronomist', 'airbrushed', 'airbrushes', 'airdropped', 'airlifting', 'airmailing', 'airmanship', 'alarmingly', 'alchemical', 'alchemists', 'alcoholics', 'alcoholism', 'algorithms', 'alienating', 'alienation', 'alignments', 'alimentary', 'alkalizing', 'allegation', 'allegiance', 'allegories', 'allegorist', 'allergenic', 'allergists', 'alleviated', 'alleviates', 'alleviator', 'alligators', 'alliterate', 'allocating', 'allocation', 'allotments', 'allowances', 'allurement', 'alluringly', 'allusively', 'almightily', 'alongshore', 'alphabetic', 'alteration', 'alterative', 'alternated', 'alternates', 'alternator', 'altimeters', 'altogether', 'altruistic', 'aluminized', 'aluminizes', 'amalgamate', 'amanuenses', 'amanuensis', 'amassments', 'amateurish', 'amateurism', 'ambassador', 'ambivalent', 'ambulances', 'ambulating', 'ambulators', 'ambulatory', 'ambuscaded', 'ameliorate', 'amendments', 'amiability', 'ammunition', 'amortizing', 'ampersands', 'amphibians', 'amphibious', 'amplifiers', 'amplifying', 'amplitudes', 'amputating', 'amputation', 'amusements', 'analgesics', 'analytical', 'analyzable', 'anarchical', 'anarchists', 'anatomical', 'anatomists', 'anatomizes', 'ancestress', 'ancestries', 'anchorages', 'androgenic', 'anemometer', 'anesthesia', 'anesthetic', 'angiosperm', 'angleworms', 'anguishing', 'angularity', 'anklebones', 'annexation', 'annihilate', 'annotating', 'annotation', 'annotative', 'announcers', 'announcing', 'annoyances', 'annoyingly', 'annualized', 'annuitants', 'annularity', 'annullable', 'annulments', 'anodically', 'anointment', 'answerable', 'antagonism', 'antagonist', 'antagonize', 'antecedent', 'antechoirs', 'anteriorly', 'anthracite', 'anthropoid', 'antibiotic', 'antibodies', 'antibusing', 'anticipate', 'anticlimax', 'anticlinal', 'anticlines', 'antifungal', 'antiheroes', 'antiheroic', 'antimatter', 'antipastos', 'antiphonal', 'antipodean', 'antiproton', 'antiquated', 'antiquates', 'antiseptic', 'antiserums', 'antisocial', 'antitheses', 'antithesis', 'antithetic', 'antonymous', 'apartments', 'aphoristic', 'apocalypse', 'apocryphal', 'apolitical', 'apologetic', 'apologists', 'apologized', 'apologizer', 'apologizes', 'apoplectic', 'apostasies', 'apostatize', 'apostrophe', 'apotheoses', 'apotheosis', 'appareling', 'apparelled', 'apparently', 'apparition', 'appearance', 'appellants', 'appendages', 'appendices', 'appendixes', 'appertains', 'appetizers', 'appetizing', 'applauders', 'applauding', 'applesauce', 'appliances', 'applicable', 'applicably', 'applicants', 'applicator', 'appointees', 'appointing', 'appointive', 'apportions', 'appositely', 'appraisals', 'appraisers', 'appreciate', 'apprehends', 'apprentice', 'approached', 'approaches', 'aquamarine', 'aquaplaned', 'aquaplanes', 'arabesques', 'arbitrager', 'arbitrated', 'arbitrates', 'arbitrator', 'arboretums', 'arborvitae', 'archaistic', 'archangels', 'archetypal', 'archetypes', 'archetypic', 'architects', 'archivists', 'archonship', 'aristocrat', 'arithmetic', 'armadillos', 'armistices', 'arraigning', 'arrogantly', 'arrogating', 'artfulness', 'arthritics', 'arthropods', 'artichokes', 'articulate', 'artificers', 'artificial', 'ascendancy', 'ascensions', 'ascertains', 'asceticism', 'ascribable', 'ascription', 'asexuality', 'aspersions', 'asphyxiate', 'aspidistra', 'aspiration', 'aspirators', 'aspiringly', 'assailable', 'assailants', 'assaulters', 'assaulting', 'assemblage', 'assemblers', 'assemblies', 'assembling', 'assertions', 'assessable', 'assessment', 'assignable', 'assignment', 'assimilate', 'assistance', 'assistants', 'associated', 'associates', 'assonances', 'assonantly', 'assortment', 'assumption', 'assumptive', 'assurances', 'asteroidal', 'asthmatics', 'astigmatic', 'astonished', 'astonishes', 'astounding', 'astringent', 'astrologer', 'astronauts', 'astronomer', 'astronomic', 'astuteness', 'asymmetric', 'asymptotes', 'asymptotic', 'athenaeums', 'atmosphere', 'atomically', 'atonements', 'atrocities', 'attachable', 'attachment', 'attainable', 'attainders', 'attainment', 'attempters', 'attempting', 'attendance', 'attendants', 'attentions', 'attenuated', 'attractant', 'attracting', 'attraction', 'attractive', 'attributed', 'attributes', 'atypically', 'auctioneer', 'auctioning', 'audibility', 'audiophile', 'audiotapes', 'auditioned', 'auditorium', 'augmenters', 'augmenting', 'auscultate', 'auspicious', 'authorized', 'authorizes', 'authorship', 'autoclaves', 'autocratic', 'autographs', 'automatics', 'automating', 'automation', 'automatism', 'automatize', 'automatons', 'automobile', 'automotive', 'autonomous', 'avalanches', 'avaricious', 'avengingly', 'aviatrixes', 'avocations', 'awakenings', 'backboards', 'backdating', 'backgammon', 'background', 'backhanded', 'backlashes', 'backlogged', 'backpacked', 'backpacker', 'backslider', 'backslides', 'backspaced', 'backspaces', 'backstroke', 'backtracks', 'backwardly', 'backwashes', 'badinaging', 'badmouthed', 'bafflement', 'bagatelles', 'bailiwicks', 'balderdash', 'balladeers', 'ballasting', 'ballistics', 'ballooning', 'ballyhooed', 'balustered', 'balustrade', 'bandicoots', 'bandstands', 'bandwagons', 'banishment', 'bankrolled', 'bankruptcy', 'bankrupted', 'banqueters', 'banqueting', 'barbarians', 'barbarisms', 'barbarized', 'barbarizes', 'barbecuing', 'barberries', 'barbershop', 'barehanded', 'bareheaded', 'bargainers', 'bargaining', 'barkeepers', 'barkentine', 'barnstorms', 'barometers', 'barometric', 'baronesses', 'barrelling', 'barricaded', 'barricader', 'barricades', 'bartenders', 'baselessly', 'basketball', 'basketfuls', 'bassoonist', 'bastardize', 'battalions', 'battlement', 'battleship', 'bayberries', 'bayoneting', 'bayonetted', 'beachheads', 'beaconless', 'beastliest', 'beatifying', 'beautician', 'beautified', 'beautifier', 'becomingly', 'bedeviling', 'bedspreads', 'bedsprings', 'beforehand', 'befriended', 'befuddlers', 'befuddling', 'beginnings', 'begrudging', 'behavioral', 'behindhand', 'bejeweling', 'bejewelled', 'belaboring', 'beleaguers', 'believable', 'belittlers', 'belittling', 'bellwether', 'bellyached', 'bellyaches', 'belongings', 'belvederes', 'benefactor', 'beneficent', 'beneficial', 'benefiting', 'benefitted', 'benevolent', 'bequeathed', 'beseechers', 'beseeching', 'besmearing', 'bespangles', 'besprinkle', 'bestiality', 'bestialize', 'bestiaries', 'bestirring', 'bestridden', 'bestriding', 'betokening', 'betrothals', 'betrothing', 'betterment', 'bewildered', 'bewitching', 'bichloride', 'bicyclists', 'biennially', 'bifurcated', 'bifurcates', 'billboards', 'billionths', 'binoculars', 'biochemist', 'biographer', 'biographic', 'biological', 'biologists', 'biophysics', 'biotically', 'bipartisan', 'birthmarks', 'birthrates', 'birthright', 'birthstone', 'bisections', 'bisexually', 'bitterness', 'blackballs', 'blackbirds', 'blackboard', 'blackeners', 'blackening', 'blackguard', 'blackheads', 'blackjacks', 'blacklists', 'blackmails', 'blandished', 'blandisher', 'blandishes', 'blanketing', 'blasphemed', 'blasphemer', 'blasphemes', 'blathering', 'blemishing', 'blindfolds', 'blissfully', 'blistering', 'blitheness', 'blithesome', 'blitzkrieg', 'blockaders', 'blockading', 'blockheads', 'blockhouse', 'bloodhound', 'bloodiness', 'bloodlines', 'blossoming', 'blotchiest', 'blubbering', 'bludgeoned', 'bluefishes', 'bluepoints', 'blueprints', 'blunderers', 'blundering', 'blushfully', 'blusterers', 'blustering', 'boardwalks', 'boastfully', 'boastingly', 'boatswains', 'bobsledded', 'bobsledder', 'bodyguards', 'boisterous', 'bolsterers', 'bolstering', 'bombardier', 'bombarding', 'bombsights', 'bondholder', 'bonefishes', 'bookbinder', 'bookkeeper', 'bookmakers', 'bookmaking', 'bookseller', 'bookstores', 'boomerangs', 'boondoggle', 'bootblacks', 'bootlegged', 'bootlegger', 'bootlessly', 'bootlicked', 'bootlicker', 'borderline', 'bothersome', 'bottleneck', 'bottomless', 'boulevards', 'bouncingly', 'boundaries', 'bountyless', 'bowstrings', 'boycotting', 'boyfriends', 'boyishness', 'bracketing', 'braininess', 'brainstorm', 'brandished', 'brandisher', 'brandishes', 'brasseries', 'brassieres', 'brattiness', 'brawniness', 'brazenness', 'breadboard', 'breadfruit', 'breakdowns', 'breakfasts', 'breakfront', 'breakpoint', 'breakwater', 'breastbone', 'breastwork', 'breathable', 'breathiest', 'breathless', 'breeziness', 'bricklayer', 'bridegroom', 'bridesmaid', 'bridgeable', 'bridgehead', 'bridgework', 'briefcases', 'brigandage', 'brigantine', 'brightened', 'brightener', 'brightness', 'brilliance', 'brilliancy', 'briquettes', 'bristliest', 'broadcasts', 'broadening', 'broadsides', 'broadsword', 'brochettes', 'brokenness', 'brokerages', 'bronchitic', 'bronchitis', 'browbeaten', 'brownstone', 'brutalized', 'brutalizes', 'buccaneers', 'buckboards', 'buckleless', 'buckwheats', 'budgerigar', 'buffaloing', 'buffoonery', 'buffoonish', 'bugbearish', 'bulldogged', 'bulldozers', 'bulldozing', 'bullheaded', 'bumpkinish', 'burdensome', 'bureaucrat', 'burgeoning', 'burglaries', 'burglarize', 'burlesqued', 'burlesques', 'burnishers', 'burnishing', 'bushwhacks', 'businesses', 'busybodies', 'butcheries', 'butchering', 'butterfish', 'buttermilk', 'buttressed', 'buttresses', 'bystanders', 'cablegrams', 'cacciatore', 'cadaverous', 'cajoleries', 'calabooses', 'calamities', 'calamitous', 'calcifying', 'calcimined', 'calcimines', 'calculable', 'calculably', 'calculated', 'calculates', 'calculator', 'calculuses', 'calendared', 'calendered', 'calibrated', 'calibrates', 'calibrator', 'calipering', 'callousing', 'callowness', 'calumnious', 'camouflage', 'campaigned', 'campaigner', 'campaniles', 'camphorate', 'cancelable', 'cancelling', 'candescent', 'candidates', 'candidness', 'candlewick', 'cannonaded', 'cannonades', 'cannonball', 'cannoneers', 'canonicals', 'canonicity', 'canonizing', 'cantaloupe', 'cantonment', 'canvaslike', 'canvassers', 'canvassing', 'capability', 'capacitate', 'capacities', 'capacitive', 'capacitors', 'capitalism', 'capitalist', 'capitalize', 'capitulate', 'capricious', 'captaining', 'captioning', 'captiously', 'captivated', 'captivates', 'captivator', 'caramelize', 'carbonated', 'carbonates', 'carbonator', 'carbonless', 'carburized', 'carburizes', 'carcinogen', 'carcinomas', 'cardholder', 'cardinally', 'cardiogram', 'cardiology', 'carelessly', 'caretakers', 'caricature', 'carnations', 'carnivores', 'carpenters', 'carpetbags', 'carryovers', 'cartilages', 'cartooning', 'cartoonist', 'cartridges', 'cartwheels', 'caseharden', 'cashiering', 'casseroles', 'castigated', 'castigates', 'castigator', 'castrating', 'castration', 'castrators', 'casualties', 'cataclysms', 'cataleptic', 'catalogers', 'catalyzers', 'catalyzing', 'catapulted', 'catcalling', 'catchments', 'catchwords', 'catechists', 'categories', 'categorize', 'caterwauls', 'cathartics', 'cathedrals', 'catnapping', 'causticity', 'cauterizes', 'cautionary', 'cautioning', 'cautiously', 'cavalcades', 'cavalierly', 'cavitation', 'celebrants', 'celebrated', 'celebrates', 'celebrator', 'cellblocks', 'cellophane', 'cemeteries', 'censorious', 'censorship', 'censurable', 'centennial', 'centerfold', 'centigrade', 'centigrams', 'centimeter', 'centipedes', 'centralism', 'centralist', 'centralize', 'centrifuge', 'centurions', 'ceramicist', 'cerebrally', 'ceremonial', 'ceremonies', 'certifiers', 'certifying', 'certitudes', 'chagrining', 'chairwoman', 'chairwomen', 'chalkboard', 'chalkiness', 'challenged', 'challenger', 'challenges', 'chamfering', 'champagnes', 'championed', 'chancellor', 'chanceries', 'chandelier', 'changeable', 'changeless', 'changeling', 'changeover', 'channeling', 'channelize', 'channelled', 'chanteuses', 'chaperoned', 'chaplaincy', 'characters', 'charactery', 'charbroils', 'chargeable', 'charioteer', 'charitable', 'charitably', 'charlatans', 'charmingly', 'charterers', 'chartering', 'chartreuse', 'chasteners', 'chasteness', 'chastening', 'chastisers', 'chastising', 'chatelaine', 'chatterbox', 'chatterers', 'chattering', 'chattiness', 'chauffeurs', 'chauvinism', 'chauvinist', 'cheapening', 'cheatingly', 'checkbooks', 'checkering', 'checklists', 'checkmated', 'checkpoint', 'checkrooms', 'cheekiness', 'cheerfully', 'cheeriness', 'cheesiness', 'chemically', 'cherishers', 'cherishing', 'chevaliers', 'chickadees', 'chickening', 'chickweeds', 'childbirth', 'childhoods', 'childishly', 'chilliness', 'chillingly', 'chimerical', 'chimpanzee', 'chintziest', 'chiromancy', 'chiselling', 'chivalrous', 'chlorinate', 'chloroform', 'choosiness', 'chophouses', 'choppiness', 'christened', 'christener', 'chromosome', 'chronicled', 'chronicler', 'chronicles', 'chronology', 'chubbiness', 'chumminess', 'chunkiness', 'churchgoer', 'churchless', 'churchyard', 'churlishly', 'circuitous', 'circulated', 'circulates', 'circulator', 'circumcise', 'circumvent', 'civilities', 'civilizers', 'civilizing', 'clambering', 'clamminess', 'clamshells', 'clangorous', 'clannishly', 'clapboards', 'clarifiers', 'clarifying', 'classicism', 'classicist', 'classified', 'classifies', 'classmates', 'classrooms', 'clattering', 'cleanliest', 'clearances', 'clemencies', 'clerkships', 'cleverness', 'clienteles', 'clinically', 'clinicians', 'clipboards', 'cliquishly', 'clitorises', 'cloakrooms', 'clodhopper', 'cloistered', 'cloudburst', 'cloudiness', 'cloverleaf', 'clownishly', 'clumsiness', 'clustering', 'cluttering', 'coagulants', 'coagulated', 'coagulates', 'coagulator', 'coalescent', 'coalescing', 'coalitions', 'coarseness', 'coarsening', 'coastlines', 'cockatrice', 'cockfights', 'cockhorses', 'coexistent', 'coexisting', 'coffeecake', 'cofferdams', 'cognizable', 'cognizance', 'cohabiting', 'coherently', 'coiffeuses', 'coincident', 'coinciding', 'collapsing', 'collarbone', 'collarless', 'collateral', 'collations', 'colleagues', 'collecting', 'collection', 'collective', 'collectors', 'collegians', 'collegiate', 'collieries', 'collisions', 'collocated', 'collocates', 'colloquial', 'colonially', 'colonizers', 'colonnaded', 'colonnades', 'coloration', 'coloratura', 'colorcasts', 'colorfully', 'colossally', 'colossuses', 'columbines', 'columnists', 'combatants', 'combatting', 'combustion', 'combustive', 'comedienne', 'comeliness', 'comforters', 'comforting', 'comicality', 'commandant', 'commandeer', 'commanders', 'commanding', 'commencing', 'commending', 'commentary', 'commenting', 'commercial', 'commingled', 'commingles', 'commission', 'commitment', 'committals', 'committees', 'commodious', 'commodores', 'commonalty', 'commonness', 'commonweal', 'commotions', 'communally', 'communions', 'communique', 'communists', 'compacting', 'compactors', 'companions', 'comparable', 'comparably', 'comparison', 'compassion', 'compatible', 'compatibly', 'compatriot', 'compellers', 'compelling', 'compendium', 'compensate', 'competence', 'competency', 'competitor', 'complacent', 'complained', 'complainer', 'complaints', 'complected', 'complement', 'completely', 'completing', 'completion', 'complexion', 'complexity', 'compliance', 'complicate', 'complicity', 'compliment', 'components', 'comporting', 'composedly', 'composites', 'compositor', 'compounded', 'compounder', 'comprehend', 'compressed', 'compresses', 'compressor', 'comprising', 'compromise', 'compulsion', 'compulsive', 'compulsory', 'computable', 'concealers', 'concealing', 'conceivers', 'conceiving', 'conception', 'conceptual', 'concerning', 'concertina', 'concerting', 'concession', 'concierges', 'conciliate', 'concluders', 'concluding', 'conclusion', 'conclusive', 'concocting', 'concoction', 'concordant', 'concourses', 'concretely', 'concreting', 'concretion', 'concubines', 'concurrent', 'concurring', 'concussion', 'condemners', 'condemning', 'condensate', 'condensers', 'condensing', 'condescend', 'condiments', 'conditions', 'condonable', 'conducting', 'conduction', 'conductive', 'conductors', 'confection', 'conference', 'conferment', 'conferrers', 'conferring', 'confessing', 'confession', 'confessors', 'confidante', 'confidants', 'confidence', 'confirming', 'confiscate', 'conflicted', 'conformers', 'conforming', 'conformism', 'conformist', 'conformity', 'confounded', 'confounder', 'confronted', 'confusedly', 'confusions', 'congealing', 'congeneric', 'congenital', 'congesting', 'congestion', 'congestive', 'congregate', 'congresses', 'congruence', 'congruency', 'conjecture', 'conjoining', 'conjugally', 'conjugated', 'conjugates', 'conjugator', 'connecting', 'connection', 'connective', 'connectors', 'conniption', 'connivance', 'conquerors', 'conscience', 'conscripts', 'consecrate', 'consenting', 'consequent', 'conserving', 'considered', 'consignees', 'consignors', 'consistent', 'consisting', 'consonance', 'consonants', 'consorting', 'consortium', 'conspiracy', 'conspirers', 'conspiring', 'constables', 'constantly', 'constipate', 'constitute', 'constrains', 'constraint', 'constricts', 'constructs', 'construers', 'construing', 'consulates', 'consulship', 'consultant', 'consulting', 'consumable', 'consummate', 'contacting', 'contagions', 'contagious', 'containers', 'containing', 'contendere', 'contenders', 'contending', 'contenting', 'contention', 'contestant', 'contesting', 'contextual', 'contiguous', 'continence', 'continents', 'contingent', 'continuant', 'continuers', 'continuing', 'continuity', 'continuous', 'contorting', 'contortion', 'contortive', 'contouring', 'contraband', 'contracted', 'contractor', 'contradict', 'contrarily', 'contrasted', 'contravene', 'contribute', 'contritely', 'contrition', 'contrivers', 'contriving', 'controlled', 'controller', 'controvert', 'contusions', 'conundrums', 'convalesce', 'convecting', 'convection', 'convective', 'convenient', 'convention', 'conventual', 'convergent', 'converging', 'conversant', 'conversely', 'conversing', 'conversion', 'converters', 'converting', 'convertors', 'conveyance', 'convicting', 'conviction', 'convincers', 'convincing', 'convoluted', 'convulsing', 'convulsion', 'cooperated', 'cooperates', 'cooperator', 'coordinate', 'copartners', 'copperhead', 'copulating', 'copulation', 'copulative', 'copulatory', 'copyreader', 'copyrights', 'copywriter', 'cordiality', 'corianders', 'corkscrews', 'cornetists', 'cornstalks', 'cornstarch', 'coronaries', 'coronation', 'corporally', 'corralling', 'correcting', 'correction', 'correlated', 'correlates', 'correspond', 'corrodible', 'corrosives', 'corrugated', 'corrugates', 'corrugator', 'corrupting', 'corruption', 'corruptive', 'coruscates', 'cosmically', 'cosmonauts', 'cosmopolis', 'costliness', 'costumiers', 'cotangents', 'cotillions', 'cottonseed', 'councilman', 'councilmen', 'counseling', 'counselled', 'counselors', 'counteract', 'countering', 'counterman', 'countermen', 'counterspy', 'countesses', 'countryman', 'courageous', 'courtesans', 'courtesies', 'courthouse', 'courtliest', 'courtships', 'courtyards', 'couturiere', 'couturiers', 'covenanted', 'covertness', 'covetously', 'crabbiness', 'crackliest', 'craftiness', 'cragginess', 'crankcases', 'crankiness', 'crankshaft', 'cravenness', 'crayonists', 'creakiness', 'creameries', 'creaminess', 'creatively', 'creativity', 'credential', 'creditable', 'creditably', 'creepiness', 'cremations', 'crescendos', 'crescentic', 'cretaceous', 'cricketers', 'criminally', 'crimsoning', 'crinkliest', 'crinolines', 'crispiness', 'critically', 'criticisms', 'criticized', 'criticizer', 'criticizes', 'croakiness', 'crocheters', 'crocheting', 'crossbeams', 'crossbones', 'crossbreed', 'crossovers', 'crossroads', 'crosswords', 'crucifixes', 'crucifying', 'crunchiest', 'cryogenics', 'cryptogram', 'cuckolding', 'cuddlesome', 'cudgelling', 'culminated', 'culminates', 'cultivated', 'cultivates', 'cultivator', 'culturally', 'cumbersome', 'cummerbund', 'cumulating', 'cumulative', 'curatively', 'curatorial', 'curbstones', 'curmudgeon', 'currencies', 'curricular', 'curriculum', 'cursedness', 'curtailing', 'curtaining', 'curvaceous', 'curvatures', 'cushioning', 'custodians', 'customized', 'customizes', 'cutthroats', 'cuttlebone', 'cyclically', 'cyclopedia', 'cymbalists', 'daintiness', 'damagingly', 'dandelions', 'dandifying', 'dapperness', 'daredevils', 'daughterly', 'davenports', 'daydreamed', 'daydreamer', 'deactivate', 'deadliness', 'deadlocked', 'dealership', 'deathblows', 'debasement', 'debauchery', 'debauching', 'debentures', 'debilitate', 'debilities', 'debriefing', 'debutantes', 'decadently', 'decapitate', 'decathlons', 'decelerate', 'deceptions', 'deciliters', 'decimalize', 'decimating', 'decimation', 'decimeters', 'deciphered', 'decisional', 'decisively', 'decisteres', 'declaimers', 'declaiming', 'declassify', 'declension', 'declinable', 'decomposed', 'decomposer', 'decompress', 'decongests', 'decontrols', 'decorating', 'decoration', 'decorative', 'decorators', 'decorously', 'decreasing', 'decrements', 'decrepitly', 'dedicating', 'dedication', 'dedicators', 'dedicatory', 'deductible', 'deductions', 'defacement', 'defamation', 'defamatory', 'defaulters', 'defaulting', 'defeatists', 'defecating', 'defecation', 'defections', 'defendable', 'defendants', 'defensible', 'deferments', 'deferrable', 'deficiency', 'defilement', 'defilingly', 'definitely', 'definition', 'definitive', 'deflecting', 'deflection', 'deflective', 'deflectors', 'deflowered', 'defoliants', 'defoliated', 'defoliates', 'defoliator', 'deforested', 'deformable', 'defrauders', 'defrauding', 'defrayable', 'defrocking', 'defrosting', 'degaussing', 'degeneracy', 'degenerate', 'degradable', 'degradedly', 'dehumanize', 'dehydrated', 'dehydrates', 'dehydrator', 'deionizing', 'dejectedly', 'dejections', 'dekameters', 'delectable', 'delectably', 'delegating', 'delegation', 'deliberate', 'delicacies', 'delicately', 'delightful', 'delighting', 'delimiting', 'delineated', 'delineates', 'delinquent', 'deliquesce', 'deliverers', 'deliveries', 'delivering', 'delusional', 'demagogues', 'demarcator', 'dementedly', 'demobilize', 'democratic', 'demodulate', 'demography', 'demolished', 'demolishes', 'demolition', 'demonetize', 'demoniacal', 'demoralize', 'demureness', 'denigrated', 'denigrates', 'denigrator', 'denominate', 'denotation', 'denotative', 'denouement', 'denouncers', 'dentifrice', 'deoxidized', 'deoxidizer', 'department', 'departures', 'dependable', 'dependably', 'dependence', 'dependency', 'dependents', 'depictions', 'depletable', 'depletions', 'deplorable', 'deplorably', 'deployment', 'depolarize', 'depopulate', 'deportable', 'deportment', 'depositing', 'deposition', 'depositors', 'depository', 'deprecates', 'deprecator', 'depreciate', 'depressant', 'depressing', 'depression', 'depressive', 'depressors', 'deputizing', 'derailment', 'derisively', 'derivation', 'derivative', 'dermatitis', 'derogatory', 'derringers', 'desalinate', 'desalinize', 'descendant', 'descendent', 'descending', 'describers', 'describing', 'desecrated', 'desecrates', 'deselected', 'deservedly', 'desiccants', 'desiccated', 'desiccates', 'desiccator', 'desiderata', 'designated', 'designates', 'desolately', 'desolating', 'desolation', 'despairing', 'desperados', 'despicable', 'despicably', 'despoilers', 'despoiling', 'despondent', 'desponding', 'despotisms', 'destroyers', 'destroying', 'destructed', 'detachable', 'detachably', 'detachment', 'detainment', 'detectable', 'detectible', 'detectives', 'detergents', 'determined', 'determines', 'deterrence', 'deterrents', 'detestable', 'detonating', 'detonation', 'detonators', 'detoxified', 'detoxifies', 'detracting', 'detractors', 'detraining', 'devaluated', 'devaluates', 'devastated', 'devastates', 'devastator', 'developers', 'developing', 'deviancies', 'deviations', 'devilishly', 'devilments', 'deviltries', 'devitalize', 'devotional', 'devoutness', 'diabolical', 'diagnosing', 'diagnostic', 'diagonally', 'diagraming', 'diagrammed', 'dialectics', 'diaphanous', 'diaphragms', 'diathermic', 'dictations', 'dictionary', 'dielectric', 'difference', 'difficulty', 'diffidence', 'diffusions', 'digestible', 'dignifying', 'digressing', 'digression', 'digressive', 'dilatation', 'dilettante', 'diligently', 'dillydally', 'dimensions', 'diminished', 'diminishes', 'diminution', 'diminutive', 'diphtheria', 'diphtheric', 'dipsomania', 'directions', 'directives', 'directness', 'dirigibles', 'disability', 'disabusing', 'disaffects', 'disallowed', 'disappears', 'disappoint', 'disapprove', 'disarrange', 'disarrayed', 'disastrous', 'disavowals', 'disbanding', 'disbarment', 'disbarring', 'disbelieve', 'disburdens', 'disbursing', 'discarding', 'discerners', 'discerning', 'discharged', 'discharger', 'discharges', 'discipline', 'disclaimed', 'disclaimer', 'disclosing', 'disclosure', 'discolored', 'discomfort', 'discommode', 'discompose', 'disconcert', 'disconnect', 'discontent', 'discordant', 'discounted', 'discounter', 'discourage', 'discoursed', 'discourser', 'discourses', 'discovered', 'discoverer', 'discredits', 'discreetly', 'discretely', 'discretion', 'discursive', 'discussant', 'discussing', 'discussion', 'disdainful', 'disdaining', 'disembarks', 'disenchant', 'disengaged', 'disengages', 'disfigured', 'disfigures', 'disgorging', 'disgracers', 'disgracing', 'disgruntle', 'disguising', 'disgusting', 'disharmony', 'dishearten', 'disheveled', 'dishonesty', 'dishonored', 'dishtowels', 'disincline', 'disinfects', 'disinherit', 'disjoining', 'disjointed', 'dislocated', 'dislocates', 'dislodging', 'disloyally', 'disloyalty', 'dismalness', 'dismantled', 'dismantles', 'dismembers', 'dismissals', 'dismissing', 'dismounted', 'disobeyers', 'disobeying', 'disobliged', 'disobliges', 'disordered', 'disorderly', 'disparaged', 'disparages', 'dispassion', 'dispatched', 'dispatcher', 'dispatches', 'dispelling', 'dispensary', 'dispensers', 'dispensing', 'dispersals', 'dispersing', 'dispersion', 'dispirited', 'displacing', 'displaying', 'displeased', 'displeases', 'disporting', 'disposable', 'dispossess', 'disproving', 'disputable', 'disputants', 'disqualify', 'disquieted', 'disregards', 'disrespect', 'disrupting', 'disruption', 'dissatisfy', 'dissecting', 'dissection', 'dissectors', 'dissembled', 'dissembler', 'dissembles', 'dissension', 'dissenters', 'dissenting', 'disservice', 'dissevered', 'dissidence', 'dissidents', 'dissimilar', 'dissipated', 'dissipater', 'dissipates', 'dissipator', 'dissociate', 'dissolving', 'dissonance', 'dissuading', 'dissuasion', 'dissuasive', 'distancing', 'distending', 'distension', 'distention', 'distillate', 'distillers', 'distillery', 'distilling', 'distinctly', 'distorters', 'distorting', 'distortion', 'distracted', 'distressed', 'distresses', 'distribute', 'distrusted', 'disturbers', 'disturbing', 'disuniters', 'disunities', 'disuniting', 'divergence', 'diversions', 'divination', 'divinities', 'divisional', 'divisively', 'doctorates', 'documented', 'dogmatists', 'dolorously', 'domiciling', 'dominantly', 'dominating', 'domination', 'dominators', 'domineered', 'donnybrook', 'doorplates', 'doubleness', 'doubtfully', 'doubtingly', 'doughtiest', 'dovetailed', 'downfallen', 'downgraded', 'downgrades', 'downshifts', 'downsizing', 'downstairs', 'draftiness', 'dragooning', 'drainpipes', 'dramatists', 'dramatized', 'dramatizes', 'drawbridge', 'drawstring', 'dreadfully', 'dreaminess', 'dreariness', 'dressiness', 'dressmaker', 'drivelling', 'drolleries', 'droopiness', 'drossiness', 'drowsiness', 'drudgeries', 'drugstores', 'drumsticks', 'dumfounded', 'duplicated', 'duplicates', 'duplicator', 'durability', 'durational', 'dynamistic', 'dynamiters', 'dynamiting', 'earmarking', 'earthbound', 'earthiness', 'earthliest', 'earthlings', 'earthquake', 'easterners', 'eastwardly', 'eavesdrops', 'ebullience', 'ecological', 'economical', 'economists', 'economizer', 'economizes', 'ecosystems', 'ecumenical', 'edibleness', 'editorials', 'editorship', 'educations', 'effaceable', 'effectuate', 'efficiency', 'effloresce', 'effortless', 'effrontery', 'effusively', 'egocentric', 'egoistical', 'eighteenth', 'eightieths', 'ejaculated', 'ejaculates', 'ejaculator', 'elaborated', 'elaborates', 'elaborator', 'elasticize', 'elastomers', 'electively', 'electrical', 'electronic', 'elementary', 'elevations', 'eliminated', 'eliminates', 'ellipsoids', 'elliptical', 'elongating', 'elongation', 'elopements', 'eloquently', 'elucidated', 'elucidates', 'elucidator', 'emanations', 'emancipate', 'emasculate', 'embankment', 'embargoing', 'embezzlers', 'embezzling', 'embittered', 'emblazoned', 'embodiment', 'emboldened', 'embossment', 'embroiders', 'embroidery', 'embroiling', 'emergences', 'emigrating', 'emigration', 'emissaries', 'emoluments', 'empathized', 'empathizes', 'empennages', 'emphasized', 'emphasizes', 'empiricism', 'empiricist', 'employment', 'empowering', 'emulations', 'emulsified', 'emulsifier', 'emulsifies', 'enactments', 'encampment', 'encapsuled', 'encapsules', 'enchaining', 'enchanters', 'enchanting', 'enciphered', 'encircling', 'enclosable', 'enclosures', 'encounters', 'encouraged', 'encourager', 'encourages', 'encroached', 'encroaches', 'encumbered', 'encystment', 'endangered', 'endearment', 'endeavored', 'endogenous', 'endorsable', 'endoscopes', 'endoscopic', 'endowments', 'energetics', 'energizers', 'energizing', 'enervating', 'enervation', 'enervators', 'enfeebling', 'engagement', 'engagingly', 'engendered', 'engineered', 'engineless', 'engravings', 'engrossers', 'engrossing', 'enjoinders', 'enjoyments', 'enlightens', 'enlistment', 'enlivening', 'enmeshment', 'enormously', 'enraptured', 'enraptures', 'enrichment', 'enrollment', 'ensconcing', 'enshrining', 'enshrouded', 'entailment', 'entanglers', 'entangling', 'enterprise', 'entertains', 'enthralled', 'enthroning', 'enthusiasm', 'enthusiast', 'enticement', 'entireness', 'entireties', 'entombment', 'entomology', 'entourages', 'entraining', 'entrancing', 'entrapment', 'entrapping', 'entreaties', 'entreating', 'entrenched', 'entrenches', 'entrusting', 'enumerates', 'enumerator', 'enunciated', 'enunciates', 'enunciator', 'envelopers', 'enveloping', 'envenoming', 'envisaging', 'envisioned', 'epicenters', 'epicentral', 'epiglottis', 'epileptics', 'epithelial', 'epithelium', 'epitomized', 'epitomizes', 'equability', 'equalities', 'equalizers', 'equalizing', 'equanimity', 'equational', 'equatorial', 'equipoises', 'equitation', 'equivalent', 'equivocate', 'eradicable', 'eradicated', 'eradicates', 'eradicator', 'ergonomics', 'erotically', 'erotogenic', 'eruptively', 'escalating', 'escalation', 'escalators', 'escalatory', 'escapement', 'escarpment', 'esophageal', 'especially', 'esplanades', 'essentials', 'estimating', 'estimation', 'estimators', 'estrogenic', 'ethereally', 'etiolating', 'etiquettes', 'eugenicist', 'eulogistic', 'eulogizing', 'euphonious', 'euthanasia', 'evacuating', 'evacuation', 'evacuators', 'evaluating', 'evaluation', 'evaluators', 'evanescent', 'evangelism', 'evangelist', 'evaporated', 'evaporates', 'evaporator', 'eventfully', 'eventually', 'eventuated', 'eventuates', 'everglades', 'evergreens', 'everyplace', 'everything', 'everywhere', 'evidencing', 'evidential', 'eviscerate', 'evocations', 'evolutions', 'evolvement', 'exacerbate', 'exactingly', 'exaggerate', 'exaltation', 'exasperate', 'excavating', 'excavation', 'excavators', 'excellence', 'exceptions', 'excerpting', 'exchanging', 'excitation', 'excitement', 'exclaimers', 'exclaiming', 'exclusions', 'excogitate', 'excoriated', 'excoriates', 'excrements', 'excretions', 'exculpated', 'exculpates', 'excursions', 'execrating', 'execration', 'execrators', 'executable', 'executions', 'executives', 'exemptible', 'exemptions', 'exercisers', 'exercising', 'exhalation', 'exhausting', 'exhaustion', 'exhaustive', 'exhibiters', 'exhibiting', 'exhibition', 'exhibitors', 'exhilarate', 'exhumation', 'exigencies', 'exiguities', 'existences', 'exonerated', 'exonerates', 'exonerator', 'exorbitant', 'exorcisers', 'exorcising', 'exospheres', 'exothermic', 'exotically', 'expandable', 'expansions', 'expatiated', 'expatiates', 'expatriate', 'expectancy', 'expedience', 'expediency', 'expedients', 'expediters', 'expediting', 'expedition', 'expellable', 'expendable', 'experience', 'experiment', 'expertness', 'expiations', 'expiration', 'explainers', 'explaining', 'expletives', 'explicable', 'explicated', 'explicates', 'explicator', 'explicitly', 'exploiters', 'exploiting', 'explosions', 'explosives', 'exposition', 'expositors', 'expository', 'expounders', 'expounding', 'expressing', 'expression', 'expressive', 'expressway', 'expulsions', 'expurgated', 'expurgates', 'expurgator', 'extensible', 'extensions', 'extenuated', 'extenuates', 'exteriorly', 'externally', 'extinction', 'extinguish', 'extirpated', 'extirpates', 'extortions', 'extraction', 'extractors', 'extradited', 'extradites', 'extralegal', 'extraneous', 'extremists', 'extricable', 'extricated', 'extricates', 'extrusions', 'exuberance', 'exudations', 'exultation', 'exultingly', 'eyedropper', 'eyeglasses', 'eyewitness', 'fabricated', 'fabricates', 'fabricator', 'fabulously', 'facilitate', 'facilities', 'facsimiles', 'factiously', 'factitious', 'factorials', 'fairground', 'fairylands', 'faithfully', 'fallacious', 'falsehoods', 'falsifiers', 'falsifying', 'familiarly', 'fanaticism', 'fancifully', 'fantasists', 'fantasized', 'fantasizes', 'farmhouses', 'farsighted', 'fascinated', 'fascinates', 'fashioners', 'fashioning', 'fastenings', 'fastidious', 'fatalistic', 'fatalities', 'fatherhood', 'fatherland', 'fatherless', 'faultiness', 'favoritism', 'fearlessly', 'fearsomely', 'feathering', 'fecklessly', 'fecundates', 'federalism', 'federalist', 'federalize', 'federating', 'federation', 'feebleness', 'felicitate', 'felicities', 'felicitous', 'fellowship', 'femaleness', 'femininely', 'femininity', 'fermenting', 'fertilized', 'fertilizer', 'fertilizes', 'fervidness', 'fetchingly', 'fetishists', 'feudalists', 'feverishly', 'fiberboard', 'fiberglass', 'fictitious', 'fidelities', 'fieldpiece', 'fiendishly', 'fierceness', 'fifteenths', 'figuration', 'figurative', 'filibuster', 'filterable', 'filthiness', 'filtrating', 'filtration', 'finalizing', 'financiers', 'fingerings', 'fingerling', 'fingernail', 'fingertips', 'finiteness', 'firebrands', 'firebreaks', 'firebricks', 'firehouses', 'fireplaces', 'flabbiness', 'flagrantly', 'flagstones', 'flamboyant', 'flameproof', 'flamingoes', 'flashbacks', 'flashbulbs', 'flashiness', 'flashlight', 'flatteners', 'flattening', 'flatterers', 'flatteries', 'flattering', 'flatulence', 'flavorings', 'flavorless', 'flavorsome', 'flawlessly', 'fledglings', 'fleeciness', 'fleetingly', 'fleshliest', 'flickering', 'flightiest', 'flightless', 'flimsiness', 'flirtation', 'flirtingly', 'floodgates', 'floodlight', 'flotations', 'floundered', 'flourished', 'flourishes', 'flowerless', 'flowerpots', 'fluctuated', 'fluctuates', 'fluffiness', 'fluoresced', 'fluoresces', 'fluoridate', 'fluorinate', 'flustering', 'flutterers', 'fluttering', 'folklorist', 'followings', 'foodstuffs', 'footboards', 'footlights', 'footlocker', 'footnoting', 'footprints', 'footstools', 'forbearers', 'forbearing', 'forbidding', 'forcefully', 'forearming', 'foreboding', 'forecasted', 'forecaster', 'forecastle', 'foreclosed', 'forecloses', 'foredoomed', 'forefather', 'forefinger', 'foreground', 'forehanded', 'foreigners', 'foreladies', 'foreordain', 'forerunner', 'foreseeing', 'foreshadow', 'foresheets', 'forestalls', 'foretasted', 'foretastes', 'foreteller', 'forewarned', 'forfeiting', 'forfeiture', 'forgetting', 'formalized', 'formalizes', 'formations', 'formatting', 'formidable', 'formidably', 'formulated', 'formulates', 'formulator', 'fornicated', 'fornicates', 'fornicator', 'forthright', 'fortifiers', 'fortifying', 'fortnights', 'fortresses', 'fortuities', 'fortuitous', 'forwarders', 'forwarding', 'fossilized', 'fossilizes', 'fosterling', 'foundation', 'foundering', 'foundlings', 'fourteenth', 'fractional', 'fracturing', 'fragmented', 'fragrances', 'fragrantly', 'frameworks', 'franchised', 'franchisee', 'franchiser', 'franchises', 'fraternity', 'fraternize', 'fraudulent', 'freakishly', 'freebooter', 'freeholder', 'freeloaded', 'freeloader', 'freighters', 'frequented', 'frequenter', 'frequently', 'fresheners', 'freshening', 'friability', 'fricasseed', 'fricassees', 'frictional', 'friendless', 'friendlier', 'friendship', 'frightened', 'fringeless', 'friskiness', 'fritterers', 'frittering', 'frizziness', 'frolickers', 'frolicking', 'frolicsome', 'frostbites', 'frostiness', 'frothiness', 'frowziness', 'fruiterers', 'fruitfully', 'fruitiness', 'frustrated', 'frustrates', 'fugitively', 'fulfillers', 'fulfilling', 'fulminated', 'fulminates', 'functional', 'functioned', 'funereally', 'fungicidal', 'fungicides', 'funnelling', 'furloughed', 'furnishing', 'furthering', 'futureless', 'futuristic', 'futurities', 'gabardines', 'gaberdines', 'gadgeteers', 'gainsayers', 'gainsaying', 'gallivants', 'gallstones', 'galvanized', 'galvanizer', 'galvanizes', 'gambolling', 'gamekeeper', 'gamesomely', 'ganglionic', 'gangplanks', 'gangrenous', 'garishness', 'garlanding', 'garnisheed', 'garnishees', 'garnishing', 'garnitures', 'garrisoned', 'gatekeeper', 'gatherings', 'gaucheries', 'gazetteers', 'gelatinous', 'gemologist', 'generality', 'generalize', 'generating', 'generation', 'generative', 'generators', 'generosity', 'generously', 'geneticist', 'gentlefolk', 'gentleness', 'genuflects', 'geocentric', 'geodesists', 'geographer', 'geographic', 'geometries', 'germinated', 'germinates', 'gestations', 'ghastliest', 'ghettoizes', 'ghostwrite', 'gingersnap', 'girlfriend', 'glaciology', 'gladdening', 'gladiators', 'glamorized', 'glamorizes', 'glamourous', 'glassiness', 'glimmering', 'glistening', 'glittering', 'globalists', 'gloominess', 'glorifiers', 'gloriously', 'glossaries', 'glossiness', 'gluttonous', 'goldbricks', 'goldenrods', 'goldfishes', 'gondoliers', 'gonococcal', 'gonococcic', 'gonorrheal', 'gorgeously', 'gormandize', 'governable', 'governance', 'government', 'gracefully', 'graciously', 'gradations', 'graduating', 'graduation', 'graduators', 'graininess', 'grammarian', 'grandchild', 'grandniece', 'grandstand', 'granduncle', 'granularly', 'granulated', 'granulates', 'granulator', 'grapefruit', 'grapevines', 'graspingly', 'grasslands', 'gratefully', 'gratifying', 'gratuities', 'gratuitous', 'gravestone', 'graveyards', 'gravimeter', 'gravitated', 'gravitates', 'graybeards', 'greasiness', 'greediness', 'greenbacks', 'greenhorns', 'greenhouse', 'greensward', 'gregarious', 'grenadiers', 'grenadines', 'greyhounds', 'grievances', 'grievously', 'grindingly', 'grindstone', 'grippingly', 'grogginess', 'grotesques', 'grouchiest', 'groundless', 'groundling', 'groundwork', 'grovelling', 'grubbiness', 'grudgingly', 'gruelingly', 'gruesomely', 'grumpiness', 'gruntingly', 'guaranteed', 'guarantees', 'guaranties', 'guarantors', 'guardhouse', 'guidebooks', 'guidelines', 'guillotine', 'guiltiness', 'gunfighter', 'gunslinger', 'gutturally', 'gymnasiums', 'gymnastics', 'gynecology', 'gyroscopes', 'habitation', 'habitually', 'habituated', 'habituates', 'hailstones', 'hailstorms', 'haircloths', 'hairspring', 'hairweaver', 'hallelujah', 'halogenoid', 'halogenous', 'hamburgers', 'hammerhead', 'hamstrings', 'handclasps', 'handcrafts', 'handedness', 'handicraft', 'handlebars', 'handpicked', 'handshakes', 'handsomely', 'handspring', 'handstands', 'handwrites', 'happenings', 'haranguing', 'harassment', 'harbingers', 'hardcovers', 'hardheaded', 'hardstands', 'harelipped', 'harmlessly', 'harmonicas', 'harmonious', 'harmonized', 'harmonizer', 'harmonizes', 'harnessers', 'harnessing', 'harpooners', 'harpooning', 'harvesters', 'harvesting', 'hatcheries', 'haughtiest', 'hauntingly', 'headboards', 'headhunter', 'headlights', 'headlining', 'headmaster', 'headphones', 'headpieces', 'headstalls', 'headstones', 'headstrong', 'headwaters', 'healthiest', 'heartaches', 'heartbeats', 'heartbreak', 'heartburns', 'heartening', 'hearthside', 'heartiness', 'heathendom', 'heathenish', 'heathenism', 'heatstroke', 'heavenward', 'hedonistic', 'heedlessly', 'hegemonies', 'heightened', 'helicopter', 'heliotrope', 'helplessly', 'henceforth', 'heraldists', 'heraldries', 'herbaceous', 'herbalists', 'herbicidal', 'herbicides', 'herbivores', 'herdswoman', 'herdswomen', 'hereditary', 'heredities', 'heretofore', 'hermitages', 'herniating', 'heroically', 'hesitantly', 'hesitaters', 'hesitating', 'hesitation', 'hibernated', 'hibernates', 'hibernator', 'hiccupping', 'hierarchic', 'highlander', 'highlights', 'highnesses', 'hightailed', 'highwayman', 'highwaymen', 'hindrances', 'hinterland', 'histaminic', 'histograms', 'historians', 'historical', 'histrionic', 'hitchhiked', 'hitchhiker', 'hitchhikes', 'hoarseness', 'hobbyhorse', 'hobgoblins', 'hobnobbing', 'hodgepodge', 'hollowness', 'hollowware', 'hollyhocks', 'holocausts', 'holographs', 'holography', 'holystones', 'homebodies', 'homecoming', 'homeliness', 'homemakers', 'homemaking', 'homeowners', 'homesteads', 'homogenize', 'homosexual', 'honestness', 'honeycombs', 'honeymoons', 'honorarily', 'honorarium', 'hoodwinked', 'hopelessly', 'horehounds', 'horizontal', 'horologist', 'horrendous', 'horridness', 'horrifying', 'horseflesh', 'horseflies', 'horsehides', 'horselaugh', 'horseshoer', 'horseshoes', 'horsetails', 'horsewoman', 'horsewomen', 'hospitable', 'hospitably', 'hostelries', 'houseflies', 'households', 'housemaids', 'housewares', 'housewives', 'humaneness', 'humanistic', 'humanities', 'humanizers', 'humanizing', 'humbleness', 'humdingers', 'humidified', 'humidifier', 'humidifies', 'humiliated', 'humiliates', 'humorously', 'hundredths', 'hungerless', 'huntresses', 'hurricanes', 'husbanding', 'husbandman', 'husbandmen', 'hybridized', 'hybridizer', 'hybridizes', 'hydrations', 'hydraulics', 'hydrometer', 'hydrophone', 'hydroplane', 'hydroponic', 'hygienists', 'hygrometer', 'hygrometry', 'hyperbaric', 'hyperbolas', 'hyperboles', 'hyperbolic', 'hyphenated', 'hyphenates', 'hypnotists', 'hypnotized', 'hypnotizes', 'hypocenter', 'hypocrites', 'hypothesis', 'hysterical', 'iconoclasm', 'iconoclast', 'idealistic', 'idealizing', 'ideational', 'identified', 'identifies', 'identities', 'ideologies', 'idolatries', 'idolatrous', 'ignobility', 'ignorantly', 'illegality', 'illiteracy', 'illiterate', 'illuminate', 'illustrate', 'imaginable', 'imaginably', 'imbalances', 'imbecility', 'imbroglios', 'imitations', 'immaculate', 'immanently', 'immaterial', 'immaturely', 'immaturity', 'immemorial', 'immersions', 'immigrants', 'immigrated', 'immigrates', 'imminently', 'immobility', 'immobilize', 'immoderacy', 'immoderate', 'immodestly', 'immolating', 'immolation', 'immorality', 'immortally', 'immunities', 'immunizing', 'impairment', 'impalement', 'impalpable', 'impalpably', 'impaneling', 'impanelled', 'impassable', 'impatience', 'impeachers', 'impeaching', 'impeccable', 'impeccably', 'impedances', 'impediment', 'imperative', 'imperfects', 'imperiling', 'impervious', 'impishness', 'implacable', 'implacably', 'implanting', 'implements', 'implicated', 'implicates', 'implicitly', 'implosions', 'impolitely', 'importable', 'importance', 'importuned', 'importunes', 'imposingly', 'imposition', 'impossible', 'impostures', 'impotently', 'impounding', 'impoverish', 'imprecated', 'imprecates', 'imprecator', 'impregnate', 'impressers', 'impressing', 'impression', 'impressive', 'imprimatur', 'imprinters', 'imprinting', 'imprisoned', 'improbable', 'improbably', 'improperly', 'improvised', 'improviser', 'improvises', 'improvisor', 'imprudence', 'impudently', 'impulsions', 'impureness', 'impurities', 'inaccuracy', 'inaccurate', 'inactivate', 'inactivity', 'inadequacy', 'inadequate', 'inaugurate', 'inbreeding', 'incarnated', 'incarnates', 'incautious', 'incendiary', 'incentives', 'incestuous', 'inchoately', 'incidental', 'incinerate', 'incipience', 'incisively', 'incitation', 'incitement', 'inclemency', 'inclinable', 'inclusions', 'incoherent', 'incomplete', 'increasers', 'increasing', 'incredible', 'incredibly', 'increments', 'incubating', 'incubation', 'incubative', 'incubators', 'inculcated', 'inculcates', 'inculpable', 'inculpates', 'incumbency', 'incumbents', 'incursions', 'indecently', 'indecision', 'indecisive', 'indecorous', 'indefinite', 'indelicacy', 'indelicate', 'indentured', 'indentures', 'indicating', 'indication', 'indicative', 'indicators', 'indictable', 'indictment', 'indigently', 'indirectly', 'indiscreet', 'indisposed', 'indistinct', 'individual', 'indolently', 'inducement', 'inductance', 'inductions', 'indulgence', 'industrial', 'industries', 'inebriated', 'inebriates', 'ineligible', 'ineligibly', 'ineptitude', 'inequality', 'inequities', 'inevitable', 'inevitably', 'inexorable', 'inexorably', 'inexpertly', 'infallible', 'infallibly', 'infantries', 'infeasible', 'infections', 'infectious', 'inferences', 'infernally', 'infidelity', 'infielders', 'infighters', 'infighting', 'infiltrate', 'infinitely', 'infinities', 'infinitive', 'infinitude', 'inflatable', 'inflations', 'inflecting', 'inflection', 'inflexible', 'inflexibly', 'inflicting', 'infliction', 'influenced', 'influences', 'informally', 'informants', 'infraction', 'infrequent', 'infringers', 'infringing', 'infuriated', 'infuriates', 'inglorious', 'ingratiate', 'ingredient', 'inhabitant', 'inhabiting', 'inhalation', 'inhalators', 'inharmonic', 'inherently', 'inheriting', 'inheritors', 'inhibiting', 'inhibition', 'inhibitors', 'inhumanely', 'inhumanity', 'inimically', 'inimitable', 'inimitably', 'iniquities', 'iniquitous', 'initialing', 'initialize', 'initialled', 'initiating', 'initiation', 'initiative', 'initiators', 'initiatory', 'injections', 'injunction', 'injustices', 'innkeepers', 'innocently', 'innovating', 'innovation', 'innovative', 'innovators', 'innuendoes', 'inoculated', 'inoculates', 'inoperable', 'inordinate', 'inpatients', 'inquietude', 'inquisitor', 'insanitary', 'insatiable', 'insatiably', 'inscribers', 'inscribing', 'insecurely', 'insecurity', 'inseminate', 'insensible', 'insensibly', 'insertions', 'insightful', 'insinuated', 'insinuates', 'insinuator', 'insipidity', 'insistence', 'insistency', 'insobriety', 'insolation', 'insolently', 'insolvable', 'insolvency', 'insomniacs', 'insouciant', 'inspecting', 'inspection', 'inspectors', 'inspirited', 'installers', 'installing', 'instigated', 'instigates', 'instigator', 'instillers', 'instituted', 'instituter', 'institutes', 'institutor', 'instructed', 'instructor', 'instrument', 'insularity', 'insulating', 'insulation', 'insulators', 'insurgence', 'insurgency', 'insurgents', 'intactness', 'intangible', 'intangibly', 'integrally', 'integrated', 'integrates', 'intellects', 'intensives', 'intentions', 'intentness', 'interacted', 'interbreed', 'interceded', 'intercedes', 'intercepts', 'interclass', 'interdicts', 'interested', 'interfaced', 'interfaces', 'interfered', 'interferer', 'interferes', 'interiorly', 'interjects', 'interlaced', 'interlaces', 'interleave', 'interloped', 'interloper', 'interlopes', 'interludes', 'interlunar', 'intermarry', 'interments', 'intermixed', 'intermixes', 'internally', 'internists', 'internment', 'internodal', 'internodes', 'internship', 'interposed', 'interposer', 'interposes', 'interprets', 'interregna', 'interrupts', 'intersects', 'interstate', 'interstice', 'intertidal', 'intertwine', 'interurban', 'intervened', 'intervener', 'intervenes', 'interviews', 'interweave', 'interwoven', 'intestinal', 'intestines', 'intimacies', 'intimately', 'intimaters', 'intimating', 'intimation', 'intimidate', 'intolerant', 'intonation', 'intoxicant', 'intoxicate', 'intrastate', 'intrepidly', 'intriguers', 'introduced', 'introducer', 'introduces', 'introverts', 'intrusions', 'intuitions', 'inundation', 'inurements', 'invalidate', 'invalidity', 'invaluable', 'invaluably', 'invariable', 'invariably', 'inveighing', 'inveiglers', 'inveigling', 'inventions', 'inversions', 'invertible', 'investment', 'invigorate', 'invincible', 'invincibly', 'inviolable', 'inviolably', 'invitation', 'invocation', 'involution', 'ionization', 'ionosphere', 'iridescent', 'ironically', 'ironworker', 'irradiated', 'irradiates', 'irrational', 'irregulars', 'irrelevant', 'irresolute', 'irreverent', 'irrigating', 'irrigation', 'irrigators', 'irritating', 'irritation', 'isometrics', 'isothermal', 'italicized', 'italicizes', 'iterations', 'itinerants', 'jacarandas', 'jackanapes', 'jacketless', 'jackhammer', 'jackknifed', 'jackknifes', 'jackknives', 'jackscrews', 'jackstraws', 'jaggedness', 'jailbreaks', 'jardiniere', 'jaundicing', 'jauntiness', 'jawbreaker', 'jaywalkers', 'jaywalking', 'jealousies', 'jellybeans', 'jeopardize', 'jeopardous', 'jettisoned', 'jingoistic', 'jitterbugs', 'jobholders', 'jockstraps', 'jocoseness', 'jocularity', 'journalese', 'journalism', 'journalist', 'journeyers', 'journeying', 'journeyman', 'journeymen', 'joyfulness', 'joyousness', 'jubilantly', 'jubilation', 'judgeships', 'judgmental', 'judicatory', 'judicature', 'judicially', 'juggernaut', 'jugglingly', 'junctional', 'junketeers', 'justifying', 'juxtaposed', 'kettledrum', 'keypunched', 'keystrokes', 'kidnappers', 'kidnapping', 'kilocycles', 'kilometers', 'kindliness', 'kindnesses', 'kinematics', 'kinescopes', 'kingliness', 'knackwurst', 'knickknack', 'knighthood', 'knobbiness', 'knockwurst', 'knottiness', 'laboratory', 'laboringly', 'labyrinths', 'lacerating', 'laceration', 'lacquerers', 'lacquering', 'ladyfinger', 'lagniappes', 'lambasting', 'lamentable', 'lamentably', 'laminating', 'lamination', 'lampooners', 'lampoonery', 'lampoonist', 'landholder', 'landladies', 'landmasses', 'landowners', 'landscaped', 'landscaper', 'landscapes', 'landslides', 'languished', 'languisher', 'languishes', 'languorous', 'larcenists', 'lascivious', 'laughingly', 'launchings', 'launderers', 'laundering', 'laundryman', 'laundrymen', 'lavatories', 'lavishness', 'lawbreaker', 'lawfulness', 'leaderless', 'leadership', 'leafhopper', 'leaseholds', 'legalistic', 'legalities', 'legalizing', 'legateship', 'legibility', 'legislated', 'legislates', 'legislator', 'legitimacy', 'legitimate', 'legitimize', 'lengthened', 'lengthener', 'lengthiest', 'lengthwise', 'leprechaun', 'lesbianism', 'lethargies', 'letterhead', 'letterings', 'leviathans', 'levitating', 'levitation', 'liberalism', 'liberality', 'liberalize', 'liberating', 'liberation', 'liberators', 'libertines', 'libidinous', 'librarians', 'licensable', 'licentious', 'lieutenant', 'lifeguards', 'lifesavers', 'lifesaving', 'lifestyles', 'lighteners', 'lightening', 'lighthouse', 'lignifying', 'likability', 'likelihood', 'likenesses', 'limberness', 'limelights', 'limestones', 'limitation', 'limitative', 'limpidness', 'linguistic', 'liquefiers', 'liquefying', 'liquidated', 'liquidates', 'liquidator', 'liquidized', 'listlessly', 'literalism', 'literately', 'literature', 'lithograph', 'litigating', 'litigation', 'litigators', 'litterbugs', 'littleness', 'liturgical', 'liturgists', 'livability', 'livelihood', 'liveliness', 'liverworts', 'loadstones', 'lobotomies', 'localities', 'localizing', 'locksmiths', 'locomotive', 'lodgements', 'logistical', 'loneliness', 'lonesomely', 'longhaired', 'longitudes', 'lopsidedly', 'loquacious', 'lordliness', 'lovelessly', 'loveliness', 'lovemaking', 'loweringly', 'lubricants', 'lubricated', 'lubricates', 'lubricator', 'lugubrious', 'lukewarmly', 'lumberjack', 'lumberyard', 'luminaries', 'luminesced', 'luminesces', 'luminosity', 'luminously', 'lunchrooms', 'lusciously', 'lusterless', 'luxuriance', 'luxuriated', 'luxuriates', 'machinable', 'machinists', 'macrocosms', 'maculation', 'maelstroms', 'magistrate', 'magnetized', 'magnetizer', 'magnetizes', 'magnifiers', 'magnifying', 'magnitudes', 'mahoganies', 'maidenhead', 'maimedness', 'mainlander', 'mainliners', 'mainlining', 'mainspring', 'mainstream', 'maintained', 'maintainer', 'majestical', 'majorities', 'makeshifts', 'maladapted', 'malapropos', 'malcontent', 'malefactor', 'malevolent', 'malfeasant', 'malignancy', 'malingered', 'malingerer', 'malodorous', 'maltreated', 'manageable', 'manageably', 'management', 'managerial', 'mandamuses', 'maneuvered', 'manhandled', 'manhandles', 'manicuring', 'manicurist', 'manifested', 'manifestly', 'manifolded', 'manipulate', 'mannequins', 'mannerisms', 'manometers', 'manometric', 'manuscript', 'maraschino', 'marbleized', 'marbleizes', 'marginalia', 'marginally', 'marinating', 'marionette', 'marketable', 'marketeers', 'markswoman', 'markswomen', 'marshaling', 'martialism', 'martialist', 'martingale', 'martyrdoms', 'masculines', 'masochists', 'masquerade', 'massacrers', 'massacring', 'massagists', 'mastectomy', 'mastermind', 'masticated', 'masticates', 'mastodonic', 'masturbate', 'matchbooks', 'materially', 'maternally', 'matriarchs', 'matriarchy', 'matricides', 'mattresses', 'maturating', 'maturation', 'matureness', 'maunderers', 'mausoleums', 'maximizing', 'mayflowers', 'mayonnaise', 'mayoresses', 'meadowland', 'meadowlark', 'meagerness', 'meanderers', 'meandering', 'meaningful', 'measurable', 'measurably', 'mechanical', 'mechanisms', 'mechanists', 'mechanizer', 'mechanizes', 'meddlesome', 'medicating', 'medication', 'medievally', 'mediocrity', 'meditating', 'meditation', 'meditative', 'megacycles', 'megaphones', 'melancholy', 'mellowness', 'melodramas', 'membership', 'membranous', 'memorandum', 'memorizers', 'memorizing', 'menacingly', 'menageries', 'mendacious', 'mendicants', 'meningitis', 'meniscuses', 'menopausal', 'menstruate', 'mensurable', 'mentalists', 'mentioners', 'mentioning', 'mercantile', 'mercifully', 'mergansers', 'mesmerists', 'mesmerized', 'mesmerizer', 'messengers', 'metabolism', 'metabolite', 'metabolize', 'metaphoric', 'meteorites', 'methodical', 'methodized', 'methodizes', 'meticulous', 'metrically', 'metricized', 'metricizes', 'metronomes', 'metronomic', 'metropolis', 'mettlesome', 'microcosms', 'micrograms', 'micrograph', 'micrometer', 'microphone', 'microscope', 'microwaves', 'middlebrow', 'middlemost', 'middlingly', 'midshipman', 'midshipmen', 'midsummers', 'midwinters', 'mightiness', 'migrations', 'milestones', 'militantly', 'militaries', 'militarism', 'militarist', 'militarize', 'militiaman', 'militiamen', 'milligrams', 'milliliter', 'millimeter', 'millionths', 'millstones', 'millstream', 'millwright', 'mimeograph', 'mindlessly', 'minelayers', 'mineralize', 'mineralogy', 'minestrone', 'miniatures', 'minimalist', 'minimizers', 'minimizing', 'miniseries', 'miniskirts', 'ministered', 'ministrant', 'ministries', 'minorities', 'minuteness', 'miraculous', 'misaddress', 'misadjusts', 'misadvised', 'misadvises', 'misaligned', 'misapplied', 'misapplies', 'misarrange', 'misbehaved', 'misbehaver', 'misbehaves', 'miscalling', 'miscarried', 'miscarries', 'miscasting', 'miscellany', 'mischances', 'mischarges', 'misconduct', 'miscopying', 'miscounted', 'miscreants', 'misdealing', 'misdefined', 'misdefines', 'misdirects', 'misfortune', 'misgivings', 'misguiders', 'misguiding', 'mishandled', 'mishandles', 'mishmashes', 'misinforms', 'misjudging', 'mislabeled', 'misleading', 'mismanaged', 'mismanages', 'mismatched', 'mismatches', 'misnumbers', 'misplacing', 'misplaying', 'misprinted', 'misquoting', 'misreading', 'misshaping', 'missionary', 'misspelled', 'misstating', 'mistakable', 'mistakenly', 'mistitling', 'mistletoes', 'mistreated', 'mistresses', 'mistrusted', 'mistypings', 'mitigating', 'mitigation', 'mitigative', 'mitigatory', 'mobilizers', 'moderately', 'moderating', 'moderation', 'moderators', 'modernists', 'modernized', 'modernizer', 'modernizes', 'modernness', 'modishness', 'modulating', 'modulation', 'modulators', 'moisteners', 'moistening', 'moisturize', 'mollifiers', 'mollifying', 'monarchies', 'monarchism', 'monarchist', 'monaurally', 'monetarily', 'monitoring', 'monochrome', 'monogamist', 'monogamous', 'monographs', 'monolithic', 'monophonic', 'monopolies', 'monopolist', 'monopolize', 'monotonous', 'monumental', 'moonlights', 'moonshiner', 'moonstones', 'moonstruck', 'moralistic', 'moralities', 'moralizers', 'moratorium', 'morbidness', 'moribundly', 'morphology', 'mortarless', 'mortgagees', 'mortgaging', 'mortgagors', 'mortifying', 'mortuaries', 'mosquitoes', 'motherhood', 'motherless', 'motionless', 'motivating', 'motivation', 'motiveless', 'motorcades', 'motorcycle', 'mountebank', 'mournfully', 'moustaches', 'mouthpiece', 'mudslinger', 'multifaced', 'multiplied', 'multiplier', 'multiplies', 'multistory', 'multitudes', 'mummifying', 'munificent', 'mushroomed', 'musicianly', 'musketeers', 'mutational', 'mutilating', 'mutilation', 'mutilators', 'mutinously', 'mycologist', 'myopically', 'mysterious', 'mystically', 'mystifiers', 'mystifying', 'namelessly', 'nameplates', 'nanosecond', 'narcissism', 'narcissist', 'narrations', 'narratives', 'narrowness', 'natalities', 'nationally', 'nationwide', 'nativities', 'naturalist', 'naturalize', 'naughtiest', 'nauseating', 'nautically', 'nautiluses', 'navigating', 'navigation', 'navigators', 'nebulously', 'necromancy', 'necropolis', 'nectarines', 'needlessly', 'negatively', 'negativing', 'negativism', 'neglectful', 'neglecting', 'negligence', 'negligible', 'negotiable', 'negotiated', 'negotiates', 'negotiator', 'neighbored', 'neighborly', 'neonatally', 'nepotistic', 'nethermost', 'nettlesome', 'networking', 'neutralism', 'neutralist', 'neutrality', 'neutralize', 'newfangled', 'newscaster', 'newsdealer', 'newsletter', 'newspapers', 'nicknaming', 'nigglingly', 'nightclubs', 'nightmares', 'nightrider', 'nightshade', 'nightshirt', 'nightspots', 'nihilistic', 'nimbleness', 'nineteenth', 'nitpicking', 'nitrifying', 'nobilities', 'nominating', 'nomination', 'nominative', 'nominators', 'nonactives', 'nonaligned', 'nonchalant', 'nondrinker', 'nonethical', 'nonfactual', 'nonfederal', 'nonferrous', 'nonfiction', 'nonmembers', 'nonnatives', 'nonorganic', 'nonpareils', 'nonpayment', 'nonplussed', 'nonradical', 'nonreaders', 'nonsalable', 'nonsecular', 'nonsmokers', 'nonstriker', 'nonsuccess', 'nonsupport', 'nontaxable', 'nontypical', 'nonunified', 'nonuniform', 'nonviolent', 'nonvisible', 'nonworkers', 'nonworking', 'normalized', 'normalizes', 'northerner', 'nosebleeds', 'notability', 'notarizing', 'notational', 'noteworthy', 'noticeable', 'noticeably', 'nourishers', 'nourishing', 'novelettes', 'novelistic', 'nucleating', 'nucleation', 'nucleators', 'nucleonics', 'nullifying', 'numberless', 'numerating', 'numeration', 'numerators', 'numerology', 'numerously', 'nurseryman', 'nurserymen', 'nutritious', 'obdurately', 'obediently', 'obfuscated', 'obfuscates', 'obfuscator', 'obituaries', 'objections', 'objectives', 'obligating', 'obligation', 'obligatory', 'obligingly', 'obliterate', 'obsequious', 'observable', 'observance', 'obsessions', 'obsoletely', 'obstetrics', 'obstructed', 'obstructer', 'obstructor', 'obtainable', 'obtrusions', 'obtuseness', 'occasional', 'occasioned', 'occlusions', 'occultists', 'occupation', 'occurrence', 'oceangoing', 'odiousness', 'offensives', 'officially', 'officiated', 'officiates', 'offsetting', 'offsprings', 'oligarchic', 'omnipotent', 'omniscient', 'onslaughts', 'opalescent', 'opaqueness', 'operations', 'operatives', 'oppositely', 'opposition', 'oppressing', 'oppression', 'oppressive', 'oppressors', 'optimistic', 'optionally', 'oratorical', 'orchardist', 'orchestral', 'orchestras', 'ordinances', 'ordinarily', 'ordination', 'organizers', 'organizing', 'orientated', 'originally', 'originated', 'originates', 'originator', 'ornamental', 'ornamented', 'ornateness', 'orneriness', 'orphanages', 'orthopedic', 'oscillated', 'oscillates', 'oscillator', 'osculating', 'osculation', 'ostensible', 'ostensibly', 'osteopaths', 'osteopathy', 'ostracized', 'ostracizes', 'outbargain', 'outbidding', 'outbluffed', 'outcropped', 'outfielded', 'outfielder', 'outfitters', 'outfitting', 'outflanked', 'outgrowing', 'outgrowths', 'outguessed', 'outguesses', 'outgunning', 'outhitting', 'outlandish', 'outlasting', 'outperform', 'outplaying', 'outrageous', 'outranging', 'outranking', 'outreasons', 'outriggers', 'outrunning', 'outscoring', 'outselling', 'outshouted', 'outsmarted', 'outspelled', 'outwearing', 'outweighed', 'outwitting', 'outworkers', 'outworking', 'overacting', 'overactive', 'overbidden', 'overbought', 'overburden', 'overcharge', 'overcoming', 'overcooked', 'overcooled', 'overdrafts', 'overeating', 'overexerts', 'overextend', 'overfilled', 'overflight', 'overflowed', 'overflying', 'overgrowth', 'overhauled', 'overheated', 'overinvest', 'overlapped', 'overloaded', 'overlooked', 'overmodest', 'overpasses', 'overpaying', 'overplayed', 'overpowers', 'overpraise', 'overpriced', 'overprices', 'overprints', 'overrating', 'overreacts', 'overridden', 'overriding', 'overruling', 'overseeing', 'overshadow', 'overshoots', 'oversights', 'oversleeps', 'overstated', 'overstates', 'overstayed', 'overstocks', 'oversupply', 'overtaking', 'overtaxing', 'overthrown', 'overthrows', 'overtopped', 'overturned', 'overvalued', 'overvalues', 'overweight', 'overwhelms', 'overworked', 'ovulations', 'ownerships', 'oxidations', 'oxygenates', 'pacemakers', 'pacemaking', 'pacesetter', 'padlocking', 'paginating', 'pagination', 'painlessly', 'palavering', 'palliating', 'palliation', 'palliative', 'palmettoes', 'palpations', 'palpitated', 'palpitates', 'paltriness', 'pancreases', 'panhandler', 'pantaloons', 'pantheists', 'pantomimed', 'pantomimes', 'pantomimic', 'paperbacks', 'paperboard', 'parachuted', 'parachutes', 'paragraphs', 'parallaxes', 'paralleled', 'paralyzers', 'paralyzing', 'paramecium', 'paramedics', 'parameters', 'parametric', 'paranoiacs', 'paraphrase', 'paraplegic', 'parasitism', 'parasitize', 'parboiling', 'parcelling', 'parchments', 'pardonable', 'pardonably', 'parenthood', 'parliament', 'paroxysmal', 'parsonages', 'partiality', 'participle', 'particular', 'partitions', 'partridges', 'passageway', 'passengers', 'passionate', 'pasteboard', 'pastelists', 'pasteurize', 'patchiness', 'paternally', 'pathfinder', 'patriarchs', 'patriarchy', 'patricians', 'patricides', 'patriotism', 'patrollers', 'patrolling', 'patronized', 'patronizer', 'patronizes', 'patterning', 'pauperized', 'pauperizes', 'pawnbroker', 'paymasters', 'peacefully', 'peacemaker', 'peculiarly', 'pedagogues', 'pedestrian', 'pediatrics', 'pedicurist', 'pedometers', 'peerlessly', 'pellagrous', 'pelletizes', 'pellucidly', 'penalizing', 'pencilling', 'penetrable', 'penetrably', 'penetrated', 'penetrates', 'penitently', 'penmanship', 'pensioners', 'pensioning', 'pentagonal', 'pentathlon', 'penthouses', 'peppercorn', 'peppermint', 'perceiving', 'percentage', 'percentile', 'perception', 'perceptive', 'perceptual', 'percipient', 'percolated', 'percolates', 'percolator', 'percussion', 'peremptory', 'perfecters', 'perfection', 'perfidious', 'perforated', 'perforates', 'perforator', 'performers', 'performing', 'pericardia', 'perihelial', 'perihelion', 'perilously', 'perimeters', 'periodical', 'peripheral', 'perishable', 'perishably', 'peristyles', 'permanence', 'permanency', 'permeating', 'permeation', 'permission', 'permissive', 'permitting', 'pernicious', 'peroration', 'perpetrate', 'perpetuate', 'perpetuity', 'perplexing', 'perplexity', 'perquisite', 'persecuted', 'persecutes', 'persecutor', 'persevered', 'perseveres', 'persistent', 'persisters', 'persisting', 'personable', 'personably', 'personages', 'personally', 'personalty', 'perspiring', 'persuaders', 'persuading', 'persuasion', 'persuasive', 'pertaining', 'pertinence', 'pertinency', 'perturbing', 'perversely', 'perversion', 'perversity', 'perverting', 'pessimists', 'pestilence', 'petitioned', 'petitioner', 'petrifying', 'petrolatum', 'phalaropes', 'pharmacies', 'pharmacist', 'phenomenal', 'phenomenon', 'pheromonal', 'philanders', 'philatelic', 'philosophy', 'phlegmatic', 'phonograph', 'phosphates', 'phosphoric', 'phosphorus', 'photogenic', 'photograph', 'physically', 'physicians', 'physicists', 'physiology', 'picaresque', 'pickpocket', 'picnickers', 'picnicking', 'pictorials', 'piercingly', 'piggybacks', 'pilastered', 'pillorying', 'pillowcase', 'pillowslip', 'pilothouse', 'pinpointed', 'pinspotter', 'pinstriped', 'pinstripes', 'pioneering', 'pirouetted', 'pirouettes', 'pitchforks', 'pitilessly', 'placarders', 'placarding', 'placements', 'plagiarism', 'plagiarist', 'plagiarize', 'plaintiffs', 'plantation', 'plasterers', 'plastering', 'plasticity', 'plasticize', 'platitudes', 'playacting', 'playfellow', 'playhouses', 'playthings', 'pleasantly', 'pleasantry', 'pleasingly', 'plebiscite', 'pliability', 'ploddingly', 'pluckiness', 'plummeting', 'plunderers', 'plundering', 'pluralized', 'pluralizes', 'plutocracy', 'plutocrats', 'pocketbook', 'pockmarked', 'podiatrist', 'poetasters', 'poetically', 'poignantly', 'pointblank', 'poisonings', 'polarities', 'polarizing', 'polemicist', 'politeness', 'politician', 'politicize', 'politicked', 'pollinated', 'pollinates', 'pollinator', 'pollutants', 'polyesters', 'polygamist', 'polygamous', 'polygraphs', 'polymerize', 'polynomial', 'polyphonic', 'polytheism', 'polytheist', 'pontifical', 'poorhouses', 'popularity', 'popularize', 'populating', 'population', 'porousness', 'portending', 'portentous', 'portfolios', 'portioning', 'portliness', 'portrayals', 'portraying', 'positional', 'positioned', 'positively', 'possessing', 'possession', 'possessive', 'possessors', 'postdating', 'posteriors', 'posthumous', 'postilions', 'postmarked', 'postmaster', 'postmortem', 'postpartum', 'postponing', 'postscript', 'postulated', 'postulates', 'potbellied', 'potbellies', 'potentates', 'potentials', 'potentiate', 'potholders', 'poulticing', 'powerfully', 'powerhouse', 'practicing', 'pragmatism', 'pragmatist', 'prancingly', 'pranksters', 'preaccepts', 'preachment', 'preadapted', 'preadjusts', 'preappoint', 'prearrange', 'preassigns', 'prebilling', 'preblessed', 'preblesses', 'preboiling', 'precancels', 'precarious', 'precaution', 'precedable', 'precedence', 'precedents', 'precentors', 'precession', 'prechilled', 'preciosity', 'preciously', 'precipiced', 'precipices', 'precisians', 'precleaned', 'precluding', 'preclusion', 'precocious', 'preconceal', 'precondemn', 'precursors', 'precursory', 'predecease', 'predestine', 'predicated', 'predicates', 'predicting', 'prediction', 'predictive', 'predictors', 'predigests', 'predispose', 'preeminent', 'preempting', 'preemption', 'preemptive', 'preengaged', 'preexamine', 'preexisted', 'prefecture', 'preferable', 'preferably', 'preference', 'preferment', 'preferrers', 'preferring', 'prefigured', 'prefigures', 'preforming', 'preheating', 'prehensile', 'preinserts', 'prejudging', 'prejudiced', 'prejudices', 'premarital', 'premedical', 'premiering', 'prenatally', 'prenuptial', 'prepayment', 'preplanned', 'prepossess', 'prerecords', 'prescience', 'prescoring', 'prescribed', 'prescribes', 'preselects', 'presenters', 'presenting', 'preservers', 'preserving', 'presetting', 'presidency', 'presidents', 'presifting', 'presoaking', 'pressingly', 'pressuring', 'pressurize', 'presumable', 'presumably', 'presuppose', 'pretenders', 'pretending', 'pretension', 'pretesting', 'prettified', 'prettifier', 'prettifies', 'prettiness', 'prevailers', 'prevailing', 'preventing', 'prevention', 'preventive', 'previewing', 'previously', 'prewarming', 'prewashing', 'prickliest', 'pridefully', 'priesthood', 'priggishly', 'primevally', 'primitives', 'primordial', 'princeling', 'princesses', 'principals', 'principled', 'principles', 'priorities', 'prissiness', 'privateers', 'privations', 'privileged', 'privileges', 'prizefight', 'probations', 'procedural', 'procedures', 'proceeders', 'proceeding', 'processing', 'procession', 'processors', 'proclaimed', 'proclaimer', 'proclivity', 'procreated', 'procreates', 'procreator', 'prodigally', 'production', 'productive', 'profascist', 'professing', 'profession', 'professors', 'profferers', 'proffering', 'proficient', 'profitable', 'profitably', 'profiteers', 'profitless', 'profligacy', 'profligate', 'profounder', 'profoundly', 'profundity', 'progenitor', 'prognostic', 'programers', 'programing', 'programmed', 'programmer', 'progressed', 'progresses', 'prohibited', 'projectile', 'projecting', 'projection', 'projectors', 'prolapsing', 'prolonging', 'promenaded', 'promenader', 'promenades', 'prominence', 'promissory', 'promotable', 'promotions', 'promptbook', 'promptness', 'promulgate', 'pronounced', 'pronounces', 'proofreads', 'propaganda', 'propagated', 'propagates', 'propagator', 'propellant', 'propellers', 'propelling', 'propensity', 'properness', 'propertied', 'properties', 'prophecies', 'prophesied', 'prophesier', 'prophesies', 'prophetess', 'propitiate', 'proponents', 'proportion', 'propounded', 'propounder', 'proprietor', 'propulsion', 'proscribed', 'proscribes', 'prosecuted', 'prosecutes', 'prosecutor', 'proselyted', 'proselytes', 'prospected', 'prospector', 'prospectus', 'prospering', 'prosperity', 'prosperous', 'prostheses', 'prosthesis', 'prosthetic', 'prostitute', 'prostrated', 'prostrates', 'protecting', 'protection', 'protective', 'protectors', 'protesters', 'protesting', 'protestors', 'protoplasm', 'prototypes', 'protozoans', 'protracted', 'protractor', 'protruding', 'protrusile', 'protrusion', 'protrusive', 'provenance', 'proverbial', 'providence', 'provincial', 'provisions', 'prudential', 'pruriently', 'pseudonyms', 'psychology', 'psychopath', 'psychotics', 'pubescence', 'publicists', 'publicized', 'publicizes', 'publishers', 'publishing', 'pugnacious', 'pulsations', 'pulverized', 'pulverizes', 'pummelling', 'punctually', 'punctuated', 'punctuates', 'puncturing', 'punishable', 'punishment', 'punitively', 'puppeteers', 'purchasers', 'purchasing', 'purgatives', 'puritanism', 'purloiners', 'purloining', 'purporting', 'purposeful', 'purveyance', 'pussyfoots', 'putatively', 'putrefying', 'putrescent', 'putridness', 'puzzlement', 'pyramiding', 'pyrometers', 'quackeries', 'quadrangle', 'quadrantal', 'quadratics', 'quadrupled', 'quadruplet', 'quaintness', 'qualifiers', 'qualifying', 'quandaries', 'quantified', 'quantifies', 'quantities', 'quarantine', 'quarrelers', 'quarreling', 'quarrelled', 'quarreller', 'quartering', 'quartettes', 'queasiness', 'questioned', 'questioner', 'quickening', 'quicksteps', 'quintuplet', 'quirkiness', 'quitclaims', 'quotations', 'rabbinates', 'racecourse', 'racialists', 'racketeers', 'radiancies', 'radiations', 'radicalism', 'radicalize', 'radiometer', 'radiometry', 'radioscopy', 'radiosonde', 'raggedness', 'railleries', 'railroaded', 'railroader', 'rakishness', 'ramshackle', 'rancidness', 'randomized', 'randomizes', 'randomness', 'ranklingly', 'ransackers', 'ransacking', 'rationales', 'rationally', 'raunchiest', 'ravenously', 'ravishment', 'reabsorbed', 'reacceding', 'reaccented', 'reaccepted', 'reaccredit', 'reaccusing', 'reaccustom', 'reacquaint', 'reacquired', 'reacquires', 'reactivate', 'reactively', 'reactivity', 'readapting', 'readership', 'readjourns', 'readmitted', 'readopting', 'reaffirmed', 'realigning', 'realizable', 'reallocate', 'reanalyses', 'reanalysis', 'reanalyzed', 'reanalyzes', 'reanimated', 'reanimates', 'reappeared', 'reapplying', 'reappoints', 'reappraise', 'rearousing', 'rearranged', 'rearranges', 'rearrested', 'reascended', 'reasonable', 'reasonably', 'reassemble', 'reassembly', 'reasserted', 'reassessed', 'reassigned', 'reassorted', 'reassuming', 'reassuring', 'reattached', 'reattaches', 'reattained', 'reattempts', 'reawakened', 'rebellions', 'rebellious', 'rebounding', 'rebuilding', 'rebukingly', 'rebuttoned', 'recallable', 'recaptured', 'recaptures', 'receipting', 'receivable', 'recentness', 'receptacle', 'receptions', 'recessions', 'recharging', 'recharters', 'recharting', 'rechecking', 'rechristen', 'recidivism', 'recidivist', 'recidivous', 'recipients', 'reciprocal', 'recitalist', 'recitation', 'recitative', 'recklessly', 'reclaiming', 'reclassify', 'recleaning', 'reclothing', 'recognized', 'recognizes', 'recoilless', 'recollects', 'recolonize', 'recombined', 'recombines', 'recommence', 'recommends', 'recompense', 'recomposed', 'recomposes', 'recompound', 'reconciled', 'reconciler', 'reconciles', 'recondense', 'reconfirms', 'reconnects', 'reconsider', 'reconsigns', 'recontests', 'recontract', 'reconvened', 'reconvenes', 'reconverts', 'reconveyed', 'recordings', 'recordists', 'recounting', 'recoveries', 'recovering', 'recreantly', 'recreating', 'recreation', 'recrossing', 'recrowning', 'recruiters', 'recruiting', 'rectangles', 'rectifiers', 'rectifying', 'rectorates', 'recuperate', 'recurrence', 'redecorate', 'rededicate', 'redeemable', 'redefining', 'redelivers', 'redemption', 'redemptive', 'redemptory', 'redeployed', 'redeposits', 'redescribe', 'redesigned', 'redevelops', 'redigested', 'redirected', 'rediscover', 'redissolve', 'redistrict', 'redividing', 'redolently', 'redoubling', 'redounding', 'redrafting', 'redressing', 'redrilling', 'reductions', 'redundancy', 'reeducated', 'reeducates', 'reelecting', 'reelection', 'reembarked', 'reembodied', 'reembodies', 'reemerging', 'reemployed', 'reenacting', 'reenclosed', 'reencloses', 'reendowing', 'reengaging', 'reenlarged', 'reenlarges', 'reenlisted', 'reenslaved', 'reenslaves', 'reentering', 'reentrance', 'reequipped', 'reerecting', 'reevaluate', 'reexamined', 'reexamines', 'reexhibits', 'reexported', 'refashions', 'refastened', 'refereeing', 'referenced', 'references', 'referendum', 'refillable', 'refiltered', 'refinanced', 'refinances', 'refinement', 'refineries', 'refinished', 'reflecting', 'reflection', 'reflective', 'reflectors', 'refocusing', 'refocussed', 'reforested', 'reformable', 'refracting', 'refraction', 'refractive', 'refractors', 'refractory', 'refraining', 'refreezing', 'refreshers', 'refreshing', 'refundable', 'refutation', 'regardless', 'regathered', 'regenerate', 'regimental', 'regimented', 'regionally', 'registered', 'registrant', 'registrars', 'registries', 'regressing', 'regression', 'regressive', 'regressors', 'regretters', 'regrouping', 'regularity', 'regularize', 'regulating', 'regulation', 'regulative', 'regulators', 'regulatory', 'rehandling', 'rehardened', 'rehearings', 'rehearsals', 'rehearsers', 'rehearsing', 'reimbursed', 'reimburses', 'reimposing', 'reimprison', 'reincurred', 'reinducted', 'reinfected', 'reinflamed', 'reinflames', 'reinforced', 'reinforcer', 'reinforces', 'reinformed', 'reinfusing', 'reinfusion', 'reinscribe', 'reinserted', 'reinspects', 'reinstalls', 'reinstated', 'reinstates', 'reinstruct', 'reinsuring', 'reinvented', 'reinvested', 'reinviting', 'reinvoking', 'reinvolved', 'reinvolves', 'reiterated', 'reiterates', 'rejections', 'rejoinders', 'rejuvenate', 'rekindling', 'relabeling', 'relabelled', 'relational', 'relatively', 'relativity', 'relaunders', 'relaxation', 'relearning', 'relegating', 'relegation', 'relentless', 'relevantly', 'relicensed', 'relicenses', 'relighting', 'relinquish', 'relishable', 'relocating', 'relocation', 'reluctance', 'reluctancy', 'remainders', 'remarkable', 'remarkably', 'remarriage', 'remarrying', 'rematching', 'remeasured', 'remeasures', 'remediable', 'remedially', 'remediless', 'remembered', 'rememberer', 'reminisced', 'reminisces', 'remissions', 'remissness', 'remittance', 'remodelers', 'remodeling', 'remodified', 'remodifies', 'remonetize', 'remorseful', 'remortgage', 'remoteness', 'remunerate', 'renditions', 'renominate', 'renotified', 'renouncers', 'renouncing', 'renovating', 'renovation', 'renovators', 'renumbered', 'reoccurred', 'reopenings', 'reordering', 'reorganize', 'reoriented', 'repackaged', 'repackages', 'repainting', 'repairable', 'reparation', 'reparative', 'repatriate', 'repayments', 'repeatable', 'repeatedly', 'repellents', 'repentance', 'repertoire', 'repetition', 'repetitive', 'rephrasing', 'replanning', 'replanting', 'replicated', 'replicates', 'repopulate', 'reportable', 'reportedly', 'repository', 'reprehends', 'represents', 'repressing', 'repression', 'repressive', 'reprievers', 'reprieving', 'reprimands', 'reprinting', 'reproached', 'reproaches', 'reproduced', 'reproducer', 'reproduces', 'republican', 'repudiated', 'repudiates', 'repudiator', 'repugnance', 'repulsions', 'repurchase', 'reputation', 'requesters', 'requesting', 'requisites', 'reradiated', 'reradiates', 'rerecorded', 'reschedule', 'rescinding', 'rescission', 'resealable', 'researched', 'researcher', 'researches', 'resections', 'resembling', 'resentment', 'reservedly', 'reservists', 'reservoirs', 'resettling', 'resharpens', 'reshipment', 'reshipping', 'residences', 'residually', 'resignedly', 'resilience', 'resiliency', 'resistance', 'resistible', 'resistless', 'resolutely', 'resolution', 'resolvable', 'resonances', 'resonantly', 'resonating', 'resonation', 'resounding', 'respecters', 'respectful', 'respecting', 'respective', 'respelling', 'respirable', 'respirator', 'respondent', 'responders', 'responding', 'responsive', 'restacking', 'restaffing', 'restamping', 'restarting', 'restaurant', 'restlessly', 'restocking', 'restorable', 'restrained', 'restrainer', 'restraints', 'restricted', 'restudying', 'restuffing', 'resultants', 'resumption', 'resupplied', 'resupplies', 'resurfaced', 'resurfaces', 'resurgence', 'resurrects', 'retainable', 'retainment', 'retaliated', 'retaliates', 'retaliator', 'retardants', 'retardates', 'rethinking', 'rethreaded', 'reticently', 'retirement', 'retiringly', 'retouchers', 'retouching', 'retracting', 'retraction', 'retractors', 'retraining', 'retransfer', 'retransmit', 'retreading', 'retreating', 'retrenched', 'retrenches', 'retrievals', 'retrievers', 'retrieving', 'retrograde', 'retrogress', 'retrospect', 'returnable', 'reunifying', 'reutilized', 'reutilizes', 'revaluated', 'revaluates', 'revealment', 'revelation', 'revelatory', 'revengeful', 'reverenced', 'reverencer', 'reverences', 'reverently', 'reverified', 'reverifies', 'reversible', 'reversibly', 'reversions', 'revertible', 'revisiting', 'revitalize', 'revivalist', 'revocation', 'revolution', 'revolvable', 'reweighing', 'rewrapping', 'rhapsodies', 'rhetorical', 'rhinoceros', 'rhythmical', 'ricocheted', 'ridiculing', 'ridiculous', 'rightfully', 'rigorously', 'ritualists', 'roadblocks', 'roadsteads', 'rocketlike', 'roisterers', 'roisterous', 'rollicking', 'rootstocks', 'rosebushes', 'rotational', 'rottenness', 'rotundness', 'roughhewed', 'roughnecks', 'roundelays', 'roundhouse', 'routinized', 'routinizes', 'rubberized', 'rubberizes', 'rudderless', 'ruggedness', 'rumblingly', 'ruminating', 'rumination', 'ruminative', 'ruminators', 'rupturable', 'rustically', 'rustlingly', 'ruthlessly', 'sabotaging', 'sacraments', 'sacredness', 'sacrificed', 'sacrificer', 'sacrifices', 'sacristies', 'safeguards', 'sailfishes', 'saintliest', 'salamander', 'salesclerk', 'salivating', 'salivation', 'sallowness', 'saltcellar', 'salubrious', 'salutation', 'sanatorium', 'sanctified', 'sanctifier', 'sanctifies', 'sanctioned', 'sanctioner', 'sandalwood', 'sandbagged', 'sandbagger', 'sandblasts', 'sandpapers', 'sandpipers', 'sanguinary', 'sanguinely', 'sanitarian', 'sanitarium', 'sanitation', 'sanitizing', 'saprophyte', 'sapsuckers', 'sarcophagi', 'satellites', 'satirizers', 'satirizing', 'satisfiers', 'satisfying', 'saturating', 'saturation', 'sauerkraut', 'saunterers', 'sauntering', 'savageness', 'savoriness', 'saxophones', 'scabbiness', 'scallopers', 'scalloping', 'scampering', 'scandalize', 'scandalous', 'scantiness', 'scapegoats', 'scarceness', 'scarcities', 'scarecrows', 'scathingly', 'scatterers', 'scattering', 'scavengers', 'scavenging', 'scenically', 'scheduling', 'schematics', 'schillings', 'schismatic', 'scholastic', 'schoolboys', 'schoolgirl', 'schoolmarm', 'schoolmate', 'schoolyard', 'scientific', 'scientists', 'scintillas', 'scoldingly', 'scoreboard', 'scornfully', 'scoundrels', 'scowlingly', 'scrabblers', 'scrabbling', 'scramblers', 'scrambling', 'scrapbooks', 'scrappiest', 'scratchier', 'scratchily', 'scratching', 'scrawniest', 'screechier', 'screeching', 'scribblers', 'scrimpiest', 'scriveners', 'scroungers', 'scrounging', 'scrubbiest', 'scruffiest', 'scrunching', 'scrupulous', 'scrutinies', 'scrutinize', 'sculleries', 'sculptress', 'sculptural', 'sculptured', 'sculptures', 'scurrilous', 'seasonable', 'seasonably', 'seasonally', 'seasonings', 'secessions', 'secondhand', 'secretions', 'sectarians', 'sectioning', 'secularism', 'secularist', 'secularize', 'secureness', 'securities', 'sedateness', 'seductions', 'seductress', 'sedulously', 'seemliness', 'seethingly', 'segmenting', 'segregated', 'segregates', 'seismicity', 'seismology', 'selections', 'selectness', 'selflessly', 'semantical', 'semaphores', 'semblances', 'semestrial', 'semiactive', 'semiannual', 'semicircle', 'semicolons', 'semidesert', 'semifinals', 'semiformal', 'semiformed', 'seminarian', 'seminaries', 'seminormal', 'semipublic', 'semiweekly', 'semiyearly', 'senatorial', 'sensations', 'sensitized', 'sensitizes', 'sensualist', 'sensuality', 'sensuously', 'sentencing', 'sentiently', 'sentiments', 'separately', 'separating', 'separation', 'separatism', 'separatist', 'separative', 'separators', 'sepulchers', 'sepulchral', 'sequential', 'sequesters', 'serenaders', 'serenading', 'sereneness', 'serialists', 'serialized', 'serializes', 'serpentine', 'serviceman', 'servicemen', 'serviettes', 'settlement', 'seventieth', 'severeness', 'shabbiness', 'shagginess', 'shakedowns', 'shallowest', 'shamefaced', 'shamefully', 'shampooers', 'shampooing', 'shapeliest', 'sharecrops', 'sharpeners', 'sharpening', 'shattering', 'sheepishly', 'shellacked', 'sheltering', 'shepherded', 'shibboleth', 'shiftiness', 'shimmering', 'shockingly', 'shoddiness', 'shoestring', 'shoplifter', 'shorelines', 'shortbread', 'shortcakes', 'shorteners', 'shortening', 'shortfalls', 'shorthorns', 'shouldered', 'showpieces', 'showplaces', 'shrewdness', 'shrillness', 'shrinkable', 'shrinkages', 'shriveling', 'shuddering', 'shutterbug', 'shuttering', 'sickliness', 'sicknesses', 'sideboards', 'sidepieces', 'sidestroke', 'sideswiped', 'sideswipes', 'sightliest', 'sightseers', 'signalized', 'signalizes', 'signatures', 'signboards', 'signifying', 'silentness', 'silhouette', 'similarity', 'simonizing', 'simpleness', 'simpletons', 'simplicity', 'simplified', 'simplifies', 'simulating', 'simulation', 'simulative', 'simulators', 'sinfulness', 'singleness', 'singletons', 'singletree', 'singularly', 'sisterhood', 'situations', 'sixteenths', 'skateboard', 'skeletally', 'skepticism', 'sketchiest', 'skillfully', 'skimpiness', 'skinflints', 'skinniness', 'skirmished', 'skirmisher', 'skirmishes', 'skittering', 'skyjackers', 'skyjacking', 'skylarking', 'skyrockets', 'skyscraper', 'skywriters', 'skywriting', 'slackening', 'slanderers', 'slandering', 'slanderous', 'slanginess', 'slapsticks', 'slathering', 'slatternly', 'slaughters', 'sleaziness', 'sleepiness', 'sleeveless', 'slenderize', 'slightness', 'slingshots', 'slipperier', 'slithering', 'slobbering', 'sloppiness', 'slouchiest', 'sluggishly', 'slumberers', 'slumbering', 'slushiness', 'smartingly', 'smashingly', 'smattering', 'smelliness', 'smirkingly', 'smokehouse', 'smokestack', 'smoldering', 'smoothness', 'smothering', 'smuttiness', 'snarlingly', 'sneakiness', 'sneakingly', 'sneeringly', 'snickering', 'sniggering', 'snobbishly', 'snootiness', 'snowballed', 'snowflakes', 'snowmobile', 'snowstorms', 'soapstones', 'sobriquets', 'socialites', 'sociologic', 'soddenness', 'sojourners', 'sojourning', 'soldiering', 'solemnized', 'solemnizes', 'solemnness', 'solenoidal', 'soliciting', 'solicitors', 'solicitous', 'solicitude', 'solidarity', 'solidified', 'solidifies', 'solitaires', 'solubility', 'somberness', 'somebodies', 'somersault', 'somnolence', 'songstress', 'songwriter', 'sonorities', 'sonorously', 'soothingly', 'sophomores', 'sophomoric', 'soporifics', 'sordidness', 'sororities', 'soubrettes', 'soundboard', 'soundtrack', 'sourpusses', 'southerner', 'sovereigns', 'sovietized', 'sovietizes', 'spacecraft', 'spaceships', 'spacesuits', 'spacewalks', 'spacewoman', 'spacewomen', 'spaciously', 'sparseness', 'spattering', 'spearheads', 'spearmints', 'specialist', 'specialize', 'specifying', 'speciously', 'spectacles', 'speculated', 'speculates', 'speculator', 'speechless', 'speedboats', 'speleology', 'spellbinds', 'spellbound', 'spelunking', 'spheroidal', 'sphincters', 'spindliest', 'spinnakers', 'spiritedly', 'spiritless', 'spitefully', 'splashdown', 'splashiest', 'splattered', 'splendidly', 'splintered', 'splotchier', 'splotching', 'spluttered', 'spoliation', 'spoliators', 'sponsoring', 'spoonerism', 'sportively', 'sportscast', 'spotlessly', 'spottiness', 'spreadable', 'springiest', 'springtime', 'sprinklers', 'sprinkling', 'spunkiness', 'spuriously', 'sputterers', 'sputtering', 'squabbiest', 'squabblers', 'squabbling', 'squandered', 'squanderer', 'squareness', 'squashiest', 'squattiest', 'squeakiest', 'squelchers', 'squelching', 'squiggling', 'squirmiest', 'stabilized', 'stabilizer', 'stabilizes', 'stagecoach', 'staggerers', 'staggering', 'stagnating', 'stagnation', 'staircases', 'stalactite', 'stalagmite', 'stalemated', 'stalemates', 'stammerers', 'stammering', 'stampeding', 'stanchions', 'standpipes', 'starchiest', 'starfishes', 'stargazers', 'stargazing', 'starvation', 'statecraft', 'statehouse', 'stateliest', 'statements', 'statically', 'stationary', 'stationers', 'stationery', 'stationing', 'statistics', 'statuaries', 'statuesque', 'statuettes', 'steadiness', 'stealthier', 'stealthily', 'steamboats', 'steelyards', 'stenciling', 'stentorian', 'stepfather', 'stepmother', 'stereotape', 'stereotype', 'sterilized', 'sterilizer', 'sterilizes', 'stewardess', 'stickiness', 'stiffeners', 'stiffening', 'stiflingly', 'stigmatize', 'stillbirth', 'stimulants', 'stimulated', 'stimulates', 'stinginess', 'stingingly', 'stintingly', 'stipulated', 'stipulates', 'stipulator', 'stirringly', 'stockiness', 'stockpiled', 'stockpiles', 'stockyards', 'stodginess', 'stomachers', 'stomaching', 'stonewalls', 'stoneworks', 'stoopingly', 'storehouse', 'storminess', 'straddlers', 'straddling', 'stragglers', 'straggling', 'straighten', 'straighter', 'straightly', 'straitened', 'stranglers', 'strangling', 'stratagems', 'strategies', 'strategist', 'stratified', 'stratifies', 'streakiest', 'streamiest', 'streamlets', 'streamline', 'streetcars', 'strengthen', 'stretchers', 'stretchier', 'stretching', 'striations', 'strickenly', 'strictness', 'strictured', 'strictures', 'stridently', 'strikingly', 'stringency', 'stringiest', 'striplings', 'striptease', 'stronghold', 'strongroom', 'structural', 'structured', 'structures', 'strugglers', 'struggling', 'stubbiness', 'stubborner', 'stubbornly', 'studiously', 'stuffiness', 'stupefying', 'stupendous', 'sturdiness', 'stutterers', 'stuttering', 'subalterns', 'subaqueous', 'subclasses', 'subclauses', 'subdialect', 'subdivided', 'subdivides', 'subheading', 'subjecting', 'subjection', 'subjective', 'subjugated', 'subjugates', 'subjugator', 'subkingdom', 'subleasing', 'subletting', 'sublimated', 'sublimates', 'subliminal', 'submarines', 'submerging', 'submersing', 'submersion', 'submission', 'submissive', 'submitting', 'subpoenaed', 'subregions', 'subroutine', 'subscribed', 'subscriber', 'subscribes', 'subscripts', 'subsection', 'subsequent', 'subsidence', 'subsidiary', 'subsidized', 'subsidizes', 'subsisting', 'subspecies', 'substances', 'substation', 'substitute', 'substratum', 'subsystems', 'subtenants', 'subtending', 'subterfuge', 'subtitling', 'subtleness', 'subtotaled', 'subtracted', 'subtrahend', 'subvention', 'subversion', 'subversive', 'subverters', 'subverting', 'succeeders', 'succeeding', 'successful', 'succession', 'successive', 'successors', 'succinctly', 'succulency', 'succulents', 'succumbers', 'suddenness', 'sufferable', 'sufferance', 'sufferings', 'sufficient', 'suffocated', 'suffocates', 'suffragans', 'suffusions', 'sugariness', 'suggesting', 'suggestion', 'suggestive', 'sultriness', 'summarized', 'summarizes', 'summations', 'summertime', 'sunbathers', 'sunbathing', 'sunbonnets', 'sunburning', 'sunstrokes', 'supercargo', 'superhuman', 'superiorly', 'superposed', 'superposes', 'superpower', 'superseded', 'supersedes', 'supervised', 'supervises', 'supervisor', 'supperless', 'supplanted', 'supplanter', 'supplement', 'suppleness', 'suppliants', 'supplicant', 'supplicate', 'supporters', 'supporting', 'supportive', 'supposedly', 'suppressed', 'suppresses', 'surcharged', 'surcharger', 'surcharges', 'surefooted', 'surfboards', 'surmounted', 'surpassing', 'surprisers', 'surprising', 'surrealist', 'surrenders', 'surrogates', 'surrounded', 'suspecting', 'suspenders', 'suspending', 'suspension', 'suspensory', 'suspicions', 'suspicious', 'sustaining', 'sustenance', 'swaggerers', 'swaggering', 'swallowing', 'swampiness', 'swarthiest', 'swaybacked', 'sweepingly', 'sweepstake', 'sweetbread', 'sweetbrier', 'sweeteners', 'sweetening', 'sweetheart', 'sweetmeats', 'sweltering', 'swimmingly', 'switchback', 'swooningly', 'sycophancy', 'sycophants', 'syllabuses', 'syllogisms', 'symbolical', 'symbolisms', 'symbolized', 'symbolizes', 'symmetries', 'sympathies', 'sympathize', 'symphonies', 'symposiums', 'synagogues', 'syncopated', 'syncopates', 'syndicated', 'syndicates', 'synergists', 'synonymous', 'synoptical', 'synthesize', 'synthetics', 'syphilitic', 'systematic', 'systemized', 'systemizes', 'tabernacle', 'tablecloth', 'tablespoon', 'tabulating', 'tabulation', 'tabulators', 'tachometer', 'taciturnly', 'tactically', 'tacticians', 'tactlessly', 'tailgating', 'taillights', 'talebearer', 'tambourine', 'tantalized', 'tantalizer', 'tantalizes', 'tantamount', 'taperingly', 'tarantulas', 'tarnishing', 'tarpaulins', 'tasselling', 'tastefully', 'tattooists', 'tawdriness', 'taxonomist', 'tearjerker', 'technician', 'techniques', 'technology', 'telecasted', 'telecaster', 'telegraphs', 'telegraphy', 'telemeters', 'telepathic', 'telephoned', 'telephoner', 'telephones', 'telephonic', 'telescoped', 'telescopes', 'telescopic', 'televising', 'television', 'tellership', 'temperance', 'temporally', 'temporized', 'temporizer', 'temporizes', 'temptation', 'temptingly', 'tenability', 'tenantless', 'tendencies', 'tenderable', 'tenderfoot', 'tenderized', 'tenderizer', 'tenderizes', 'tenderloin', 'tenderness', 'tentacular', 'tenterhook', 'terminable', 'terminally', 'terminated', 'terminates', 'terminator', 'terminuses', 'terrariums', 'terrifiers', 'terrifying', 'terrorists', 'terrorized', 'terrorizes', 'testaments', 'testifiers', 'testifying', 'thankfully', 'theatrical', 'themselves', 'theocratic', 'theologian', 'theologies', 'theorizers', 'theorizing', 'theosophic', 'therapists', 'thereafter', 'thermistor', 'thickeners', 'thickening', 'thieveries', 'thirstiest', 'thirteenth', 'thirtieths', 'thoroughly', 'thoughtful', 'thousandth', 'threadbare', 'threadiest', 'threatened', 'threatener', 'threescore', 'threesomes', 'thresholds', 'thriftiest', 'thriftless', 'throatiest', 'thromboses', 'throttlers', 'throttling', 'throughout', 'throughput', 'throwbacks', 'thumbnails', 'thumbscrew', 'thundering', 'thunderous', 'ticklishly', 'tidewaters', 'tighteners', 'tightening', 'timberland', 'timberline', 'timekeeper', 'timelessly', 'timeliness', 'timepieces', 'timesaving', 'timeserver', 'timetables', 'timorously', 'timpanists', 'tincturing', 'tirelessly', 'tiresomely', 'titillated', 'titillates', 'toiletries', 'tolerances', 'tolerantly', 'tolerating', 'toleration', 'tolerative', 'tolerators', 'tollbooths', 'tombstones', 'tonalities', 'tongueless', 'toothaches', 'toothbrush', 'toothpaste', 'topicality', 'topography', 'torchlight', 'tormenters', 'tormenting', 'tormentors', 'torpedoing', 'torrential', 'torridness', 'tortuously', 'torturedly', 'totalities', 'touchdowns', 'touchiness', 'touchingly', 'touchstone', 'tougheners', 'toughening', 'tournament', 'tourniquet', 'toweringly', 'toxicities', 'tractional', 'trademarks', 'traditions', 'trafficked', 'trafficker', 'tragically', 'traitorous', 'trajectory', 'trammeling', 'trampoline', 'transacted', 'transcends', 'transcribe', 'transcript', 'transfixed', 'transfixes', 'transforms', 'transfused', 'transfuser', 'transfuses', 'transgress', 'transience', 'transiency', 'transients', 'transistor', 'transition', 'transitive', 'transitory', 'translated', 'translates', 'translator', 'transmutes', 'transpired', 'transpires', 'transplant', 'transports', 'transposed', 'transposes', 'transships', 'transverse', 'trapezoids', 'traumatize', 'travailing', 'traversals', 'traversing', 'travestied', 'travesties', 'treadmills', 'treasonous', 'treasurers', 'treasuries', 'treasuring', 'treatments', 'tremendous', 'trenchancy', 'trespassed', 'trespasser', 'trespasses', 'triangular', 'trickeries', 'trickiness', 'tricksters', 'triflingly', 'triggering', 'trillionth', 'tripartite', 'triplicate', 'trisecting', 'trisection', 'triturates', 'triumphant', 'triumphing', 'triviality', 'trombonist', 'troopships', 'tropically', 'trousseaus', 'trousseaux', 'truculence', 'trumpeters', 'trumpeting', 'truncating', 'truncation', 'truncheons', 'trustfully', 'truthfully', 'tubercular', 'tuberculin', 'tumbledown', 'tumbleweed', 'tumultuous', 'tunability', 'tunelessly', 'tunnellers', 'tunnelling', 'turbidness', 'turbulence', 'turnabouts', 'turnaround', 'turnbuckle', 'turnstiles', 'turntables', 'turpentine', 'turtledove', 'turtleneck', 'twentieths', 'twinighter', 'twittering', 'typewriter', 'typewrites', 'typicality', 'typography', 'tyrannical', 'tyrannized', 'tyrannizer', 'tyrannizes', 'ubiquitous', 'ulcerating', 'ulceration', 'ulteriorly', 'ultimately', 'ultimatums', 'ultrasound', 'ululations', 'unabridged', 'unabsorbed', 'unaccented', 'unaccepted', 'unactuated', 'unadjusted', 'unaffected', 'unalarming', 'unanimated', 'unanswered', 'unapparent', 'unappeased', 'unapproved', 'unaspiring', 'unassessed', 'unassigned', 'unassisted', 'unassuming', 'unattached', 'unattended', 'unattested', 'unavailing', 'unbalanced', 'unbaptized', 'unbearable', 'unbearably', 'unbeatable', 'unbecoming', 'unbeholden', 'unbeliever', 'unbiasedly', 'unbleached', 'unblinking', 'unblocking', 'unblushing', 'unbribable', 'unbuckling', 'unbudgeted', 'unburdened', 'unbuttoned', 'uncanceled', 'uncensored', 'uncensured', 'unchaining', 'unchanging', 'unchastely', 'uncheerful', 'unclasping', 'unclenched', 'unclenches', 'uncloaking', 'unclogging', 'unclothing', 'uncombined', 'uncommonly', 'unconfined', 'unconfused', 'unconsoled', 'unconsumed', 'uncontrite', 'uncoupling', 'uncovering', 'uncritical', 'uncrossing', 'uncultured', 'undeceived', 'undeclared', 'undefeated', 'undefended', 'undeniable', 'undeniably', 'underacted', 'underbelly', 'underclerk', 'undercoats', 'undercooks', 'undercover', 'underdress', 'undergoing', 'underlayer', 'underlined', 'underlines', 'underlings', 'underlying', 'undermined', 'undermines', 'underneath', 'underpants', 'underparts', 'underplays', 'underprice', 'underrated', 'underrates', 'underscore', 'undersells', 'undersexed', 'undershirt', 'undersized', 'underskirt', 'underspend', 'understand', 'understate', 'understood', 'undertaken', 'undertaker', 'undertakes', 'undertones', 'undervalue', 'underwound', 'underwrite', 'underwrote', 'undeserved', 'undesigned', 'undetached', 'undetected', 'undeterred', 'undiffused', 'undigested', 'undirected', 'undismayed', 'undisputed', 'undivulged', 'undogmatic', 'undoubting', 'undramatic', 'undressing', 'undulating', 'undulation', 'undulatory', 'unearthing', 'uneconomic', 'uneducable', 'uneducated', 'unemphatic', 'unemployed', 'unenclosed', 'unendingly', 'unendorsed', 'unenforced', 'unenriched', 'unenrolled', 'unenviable', 'unequipped', 'unerringly', 'unescorted', 'unesthetic', 'unevenness', 'uneventful', 'unexampled', 'unexcelled', 'unexciting', 'unexecuted', 'unexpected', 'unexpended', 'unexplicit', 'unexploded', 'unexplored', 'unextended', 'unfairness', 'unfaithful', 'unfamiliar', 'unfastened', 'unfathomed', 'unfeasible', 'unfeminine', 'unfettered', 'unfiltered', 'unfinished', 'unflagging', 'unflavored', 'unforeseen', 'unforested', 'unforetold', 'unforgiven', 'unforsaken', 'unfreezing', 'unfriendly', 'unfrocking', 'unfruitful', 'ungathered', 'ungenerous', 'ungenially', 'ungoverned', 'ungraceful', 'ungrateful', 'ungrounded', 'ungrudging', 'unguentary', 'unhampered', 'unhardened', 'unheralded', 'unhindered', 'unhitching', 'unholiness', 'uniformity', 'unilateral', 'unimpaired', 'unimposing', 'unimproved', 'uninfected', 'uninformed', 'uninspired', 'unintended', 'uninvested', 'uninviting', 'uninvolved', 'unionizing', 'universals', 'university', 'unjustness', 'unkindness', 'unknowable', 'unlamented', 'unlatching', 'unlawfully', 'unlearning', 'unleashing', 'unleavened', 'unlettered', 'unlicensed', 'unlifelike', 'unlimbered', 'unloosened', 'unmannerly', 'unmastered', 'unmeasured', 'unmerciful', 'unmilitary', 'unmistaken', 'unmodified', 'unmolested', 'unmorality', 'unnameable', 'unnumbered', 'unobliging', 'unobscured', 'unobserved', 'unoccupied', 'unoffended', 'unofficial', 'unordained', 'unoriginal', 'unorthodox', 'unpacified', 'unpardoned', 'unpatented', 'unplayable', 'unpleasant', 'unpleasing', 'unpoetical', 'unpolished', 'unpolluted', 'unprepared', 'unprompted', 'unproposed', 'unprovided', 'unprovoked', 'unpunctual', 'unpunished', 'unpurified', 'unquenched', 'unquotable', 'unraveling', 'unravelled', 'unreadable', 'unrealized', 'unreckoned', 'unrecorded', 'unredeemed', 'unreformed', 'unreliable', 'unreliably', 'unrelieved', 'unrepealed', 'unreplaced', 'unreported', 'unreproved', 'unrequited', 'unreserved', 'unresigned', 'unresolved', 'unreturned', 'unrevealed', 'unrevenged', 'unrewarded', 'unromantic', 'unruliness', 'unsanitary', 'unsaturate', 'unschooled', 'unscramble', 'unscreened', 'unseasoned', 'unsecluded', 'unseeingly', 'unsensible', 'unsettling', 'unshackled', 'unshackles', 'unshakable', 'unshakably', 'unsheathed', 'unsheathes', 'unshielded', 'unsilenced', 'unsinkable', 'unskillful', 'unsnapping', 'unsnarling', 'unsociable', 'unsociably', 'unsocially', 'unsolvable', 'unspecific', 'unstarched', 'unsteadily', 'unstopping', 'unstrained', 'unstressed', 'unsuitable', 'unsuitably', 'unsureness', 'unswerving', 'untalented', 'untangling', 'untasteful', 'untenanted', 'unthankful', 'unthinking', 'untidiness', 'untiringly', 'untraveled', 'untroubled', 'untrustful', 'untruthful', 'untwisting', 'unutilized', 'unverified', 'unwariness', 'unwavering', 'unweakened', 'unwearable', 'unwearably', 'unwearying', 'unwontedly', 'unworkable', 'unworkably', 'unworthily', 'unwrapping', 'unwrinkled', 'unwrinkles', 'unyielding', 'upbraiding', 'upbringing', 'upchucking', 'upholsters', 'upholstery', 'uproarious', 'upshifting', 'upstanding', 'upwardness', 'urbanizing', 'urbanology', 'urinalyses', 'urinalysis', 'urogenital', 'usableness', 'usefulness', 'usherettes', 'usurpation', 'utterances', 'uxoriously', 'vacationer', 'vaccinated', 'vaccinates', 'vaccinator', 'vacillated', 'vacillates', 'vacillator', 'valentines', 'validating', 'validation', 'valorizing', 'valorously', 'valuations', 'vandalized', 'vandalizes', 'vanquished', 'vanquisher', 'vanquishes', 'vaporizers', 'vaporizing', 'vaporously', 'variations', 'varicosity', 'varietally', 'varnishing', 'vascularly', 'vegetables', 'vegetarian', 'vegetating', 'vegetation', 'vegetative', 'vehemently', 'velocipede', 'velocities', 'venerating', 'veneration', 'vengefully', 'venomously', 'ventilated', 'ventilates', 'ventilator', 'veracities', 'verbalized', 'verbalizes', 'verifiable', 'vermifuges', 'vernacular', 'vernalized', 'vernalizes', 'versifiers', 'versifying', 'vertebrate', 'vertically', 'vertigines', 'vestibules', 'veterinary', 'vibraphone', 'vibrations', 'vicariates', 'vicinities', 'victimized', 'victimizer', 'victimizes', 'victimless', 'victorious', 'victualing', 'victualled', 'videotaped', 'videotapes', 'viewpoints', 'vigilantes', 'vigilantly', 'vigorously', 'villainess', 'villainies', 'villainous', 'vindicated', 'vindicates', 'vindicator', 'vindictive', 'violations', 'violinists', 'virginally', 'virtuosity', 'virtuously', 'virulently', 'viscerally', 'visibility', 'visitation', 'visualized', 'visualizes', 'vitalities', 'vitalizers', 'vitalizing', 'vitrifying', 'viviparity', 'viviparous', 'vocabulary', 'vocalizers', 'vocational', 'vociferate', 'vociferous', 'voicedness', 'voiceprint', 'volatility', 'volitional', 'volubility', 'volumetric', 'voluminous', 'volunteers', 'voluptuary', 'voluptuous', 'vulcanizes', 'vulgarizer', 'vulgarizes', 'vulnerable', 'vulnerably', 'wagonettes', 'wainscoted', 'waistbands', 'waistcoats', 'waitresses', 'wallflower', 'wallpapers', 'wanderings', 'wanderlust', 'wantonness', 'warehoused', 'warehouser', 'warehouses', 'warmongers', 'washboards', 'washstands', 'wastefully', 'wastelands', 'watchbands', 'watchfully', 'watchwoman', 'watchwomen', 'watchwords', 'waterborne', 'watercraft', 'waterfalls', 'waterfront', 'watermarks', 'waterproof', 'watersheds', 'waveringly', 'weaknesses', 'wealthiest', 'weaponless', 'weathering', 'weatherman', 'weathermen', 'weightiest', 'weightless', 'wellspring', 'werewolves', 'westerners', 'westernize', 'westwardly', 'whaleboats', 'whalebones', 'wharfinger', 'whatsoever', 'wheelbases', 'wheelchair', 'wheeziness', 'whensoever', 'whetstones', 'whimpering', 'whiplashes', 'whirligigs', 'whirlwinds', 'whirlybird', 'whispering', 'wholesaled', 'wholesaler', 'wholesales', 'whomsoever', 'wickedness', 'widespread', 'wildcatted', 'wildebeest', 'wilderness', 'windlasses', 'windowless', 'windowpane', 'windshield', 'windstorms', 'wingspread', 'winterizes', 'wirehaired', 'wirepuller', 'wiretapped', 'wiretapper', 'witchcraft', 'witcheries', 'withdrawal', 'withholder', 'withstands', 'witnessers', 'witnessing', 'witticisms', 'wobbliness', 'woefulness', 'womanizers', 'womanizing', 'wonderland', 'wonderment', 'wondrously', 'woodenness', 'woolliness', 'workaholic', 'workhorses', 'workingman', 'workingmen', 'worktables', 'worldliest', 'worldlings', 'worrywarts', 'worshipers', 'worshipful', 'worshipped', 'worthiness', 'worthwhile', 'woundingly', 'wrathfully', 'wretchedly', 'wriggliest', 'wristbands', 'writhingly', 'wrongdoers', 'wrongdoing', 'wrongfully', 'xenophobia', 'xenophobic', 'xerography', 'xylophones', 'yardmaster', 'yardsticks', 'yesterdays', 'yesteryear', 'youngsters', 'yourselves', 'youthfully', 'zoologists']);
var $author$project$Main$init = function (flags) {
	var dimensions = $author$project$Main$parseFlags(flags);
	return _Utils_Tuple2(
		{T: dimensions, V: '', W: false, au: 'emoclew', w: '', Y: 0, ab: 'welcome', ay: $author$project$Words$words},
		$elm$core$Platform$Cmd$none);
};
var $author$project$Main$WindowResized = function (a) {
	return {$: 6, a: a};
};
var $elm$browser$Browser$Events$Window = 1;
var $elm$browser$Browser$Events$MySub = F3(
	function (a, b, c) {
		return {$: 0, a: a, b: b, c: c};
	});
var $elm$browser$Browser$Events$State = F2(
	function (subs, pids) {
		return {bl: pids, bw: subs};
	});
var $elm$core$Dict$RBEmpty_elm_builtin = {$: -2};
var $elm$core$Dict$empty = $elm$core$Dict$RBEmpty_elm_builtin;
var $elm$browser$Browser$Events$init = $elm$core$Task$succeed(
	A2($elm$browser$Browser$Events$State, _List_Nil, $elm$core$Dict$empty));
var $elm$browser$Browser$Events$nodeToKey = function (node) {
	if (!node) {
		return 'd_';
	} else {
		return 'w_';
	}
};
var $elm$browser$Browser$Events$addKey = function (sub) {
	var node = sub.a;
	var name = sub.b;
	return _Utils_Tuple2(
		_Utils_ap(
			$elm$browser$Browser$Events$nodeToKey(node),
			name),
		sub);
};
var $elm$core$Dict$Black = 1;
var $elm$core$Dict$RBNode_elm_builtin = F5(
	function (a, b, c, d, e) {
		return {$: -1, a: a, b: b, c: c, d: d, e: e};
	});
var $elm$core$Dict$Red = 0;
var $elm$core$Dict$balance = F5(
	function (color, key, value, left, right) {
		if ((right.$ === -1) && (!right.a)) {
			var _v1 = right.a;
			var rK = right.b;
			var rV = right.c;
			var rLeft = right.d;
			var rRight = right.e;
			if ((left.$ === -1) && (!left.a)) {
				var _v3 = left.a;
				var lK = left.b;
				var lV = left.c;
				var lLeft = left.d;
				var lRight = left.e;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					0,
					key,
					value,
					A5($elm$core$Dict$RBNode_elm_builtin, 1, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, 1, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					color,
					rK,
					rV,
					A5($elm$core$Dict$RBNode_elm_builtin, 0, key, value, left, rLeft),
					rRight);
			}
		} else {
			if ((((left.$ === -1) && (!left.a)) && (left.d.$ === -1)) && (!left.d.a)) {
				var _v5 = left.a;
				var lK = left.b;
				var lV = left.c;
				var _v6 = left.d;
				var _v7 = _v6.a;
				var llK = _v6.b;
				var llV = _v6.c;
				var llLeft = _v6.d;
				var llRight = _v6.e;
				var lRight = left.e;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					0,
					lK,
					lV,
					A5($elm$core$Dict$RBNode_elm_builtin, 1, llK, llV, llLeft, llRight),
					A5($elm$core$Dict$RBNode_elm_builtin, 1, key, value, lRight, right));
			} else {
				return A5($elm$core$Dict$RBNode_elm_builtin, color, key, value, left, right);
			}
		}
	});
var $elm$core$Basics$compare = _Utils_compare;
var $elm$core$Dict$insertHelp = F3(
	function (key, value, dict) {
		if (dict.$ === -2) {
			return A5($elm$core$Dict$RBNode_elm_builtin, 0, key, value, $elm$core$Dict$RBEmpty_elm_builtin, $elm$core$Dict$RBEmpty_elm_builtin);
		} else {
			var nColor = dict.a;
			var nKey = dict.b;
			var nValue = dict.c;
			var nLeft = dict.d;
			var nRight = dict.e;
			var _v1 = A2($elm$core$Basics$compare, key, nKey);
			switch (_v1) {
				case 0:
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						A3($elm$core$Dict$insertHelp, key, value, nLeft),
						nRight);
				case 1:
					return A5($elm$core$Dict$RBNode_elm_builtin, nColor, nKey, value, nLeft, nRight);
				default:
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						nLeft,
						A3($elm$core$Dict$insertHelp, key, value, nRight));
			}
		}
	});
var $elm$core$Dict$insert = F3(
	function (key, value, dict) {
		var _v0 = A3($elm$core$Dict$insertHelp, key, value, dict);
		if ((_v0.$ === -1) && (!_v0.a)) {
			var _v1 = _v0.a;
			var k = _v0.b;
			var v = _v0.c;
			var l = _v0.d;
			var r = _v0.e;
			return A5($elm$core$Dict$RBNode_elm_builtin, 1, k, v, l, r);
		} else {
			var x = _v0;
			return x;
		}
	});
var $elm$core$Dict$fromList = function (assocs) {
	return A3(
		$elm$core$List$foldl,
		F2(
			function (_v0, dict) {
				var key = _v0.a;
				var value = _v0.b;
				return A3($elm$core$Dict$insert, key, value, dict);
			}),
		$elm$core$Dict$empty,
		assocs);
};
var $elm$core$Process$kill = _Scheduler_kill;
var $elm$core$Dict$foldl = F3(
	function (func, acc, dict) {
		foldl:
		while (true) {
			if (dict.$ === -2) {
				return acc;
			} else {
				var key = dict.b;
				var value = dict.c;
				var left = dict.d;
				var right = dict.e;
				var $temp$func = func,
					$temp$acc = A3(
					func,
					key,
					value,
					A3($elm$core$Dict$foldl, func, acc, left)),
					$temp$dict = right;
				func = $temp$func;
				acc = $temp$acc;
				dict = $temp$dict;
				continue foldl;
			}
		}
	});
var $elm$core$Dict$merge = F6(
	function (leftStep, bothStep, rightStep, leftDict, rightDict, initialResult) {
		var stepState = F3(
			function (rKey, rValue, _v0) {
				stepState:
				while (true) {
					var list = _v0.a;
					var result = _v0.b;
					if (!list.b) {
						return _Utils_Tuple2(
							list,
							A3(rightStep, rKey, rValue, result));
					} else {
						var _v2 = list.a;
						var lKey = _v2.a;
						var lValue = _v2.b;
						var rest = list.b;
						if (_Utils_cmp(lKey, rKey) < 0) {
							var $temp$rKey = rKey,
								$temp$rValue = rValue,
								$temp$_v0 = _Utils_Tuple2(
								rest,
								A3(leftStep, lKey, lValue, result));
							rKey = $temp$rKey;
							rValue = $temp$rValue;
							_v0 = $temp$_v0;
							continue stepState;
						} else {
							if (_Utils_cmp(lKey, rKey) > 0) {
								return _Utils_Tuple2(
									list,
									A3(rightStep, rKey, rValue, result));
							} else {
								return _Utils_Tuple2(
									rest,
									A4(bothStep, lKey, lValue, rValue, result));
							}
						}
					}
				}
			});
		var _v3 = A3(
			$elm$core$Dict$foldl,
			stepState,
			_Utils_Tuple2(
				$elm$core$Dict$toList(leftDict),
				initialResult),
			rightDict);
		var leftovers = _v3.a;
		var intermediateResult = _v3.b;
		return A3(
			$elm$core$List$foldl,
			F2(
				function (_v4, result) {
					var k = _v4.a;
					var v = _v4.b;
					return A3(leftStep, k, v, result);
				}),
			intermediateResult,
			leftovers);
	});
var $elm$browser$Browser$Events$Event = F2(
	function (key, event) {
		return {a0: event, a9: key};
	});
var $elm$core$Platform$sendToSelf = _Platform_sendToSelf;
var $elm$browser$Browser$Events$spawn = F3(
	function (router, key, _v0) {
		var node = _v0.a;
		var name = _v0.b;
		var actualNode = function () {
			if (!node) {
				return _Browser_doc;
			} else {
				return _Browser_window;
			}
		}();
		return A2(
			$elm$core$Task$map,
			function (value) {
				return _Utils_Tuple2(key, value);
			},
			A3(
				_Browser_on,
				actualNode,
				name,
				function (event) {
					return A2(
						$elm$core$Platform$sendToSelf,
						router,
						A2($elm$browser$Browser$Events$Event, key, event));
				}));
	});
var $elm$core$Dict$union = F2(
	function (t1, t2) {
		return A3($elm$core$Dict$foldl, $elm$core$Dict$insert, t2, t1);
	});
var $elm$browser$Browser$Events$onEffects = F3(
	function (router, subs, state) {
		var stepRight = F3(
			function (key, sub, _v6) {
				var deads = _v6.a;
				var lives = _v6.b;
				var news = _v6.c;
				return _Utils_Tuple3(
					deads,
					lives,
					A2(
						$elm$core$List$cons,
						A3($elm$browser$Browser$Events$spawn, router, key, sub),
						news));
			});
		var stepLeft = F3(
			function (_v4, pid, _v5) {
				var deads = _v5.a;
				var lives = _v5.b;
				var news = _v5.c;
				return _Utils_Tuple3(
					A2($elm$core$List$cons, pid, deads),
					lives,
					news);
			});
		var stepBoth = F4(
			function (key, pid, _v2, _v3) {
				var deads = _v3.a;
				var lives = _v3.b;
				var news = _v3.c;
				return _Utils_Tuple3(
					deads,
					A3($elm$core$Dict$insert, key, pid, lives),
					news);
			});
		var newSubs = A2($elm$core$List$map, $elm$browser$Browser$Events$addKey, subs);
		var _v0 = A6(
			$elm$core$Dict$merge,
			stepLeft,
			stepBoth,
			stepRight,
			state.bl,
			$elm$core$Dict$fromList(newSubs),
			_Utils_Tuple3(_List_Nil, $elm$core$Dict$empty, _List_Nil));
		var deadPids = _v0.a;
		var livePids = _v0.b;
		var makeNewPids = _v0.c;
		return A2(
			$elm$core$Task$andThen,
			function (pids) {
				return $elm$core$Task$succeed(
					A2(
						$elm$browser$Browser$Events$State,
						newSubs,
						A2(
							$elm$core$Dict$union,
							livePids,
							$elm$core$Dict$fromList(pids))));
			},
			A2(
				$elm$core$Task$andThen,
				function (_v1) {
					return $elm$core$Task$sequence(makeNewPids);
				},
				$elm$core$Task$sequence(
					A2($elm$core$List$map, $elm$core$Process$kill, deadPids))));
	});
var $elm$core$List$maybeCons = F3(
	function (f, mx, xs) {
		var _v0 = f(mx);
		if (!_v0.$) {
			var x = _v0.a;
			return A2($elm$core$List$cons, x, xs);
		} else {
			return xs;
		}
	});
var $elm$core$List$filterMap = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$foldr,
			$elm$core$List$maybeCons(f),
			_List_Nil,
			xs);
	});
var $elm$browser$Browser$Events$onSelfMsg = F3(
	function (router, _v0, state) {
		var key = _v0.a9;
		var event = _v0.a0;
		var toMessage = function (_v2) {
			var subKey = _v2.a;
			var _v3 = _v2.b;
			var node = _v3.a;
			var name = _v3.b;
			var decoder = _v3.c;
			return _Utils_eq(subKey, key) ? A2(_Browser_decodeEvent, decoder, event) : $elm$core$Maybe$Nothing;
		};
		var messages = A2($elm$core$List$filterMap, toMessage, state.bw);
		return A2(
			$elm$core$Task$andThen,
			function (_v1) {
				return $elm$core$Task$succeed(state);
			},
			$elm$core$Task$sequence(
				A2(
					$elm$core$List$map,
					$elm$core$Platform$sendToApp(router),
					messages)));
	});
var $elm$browser$Browser$Events$subMap = F2(
	function (func, _v0) {
		var node = _v0.a;
		var name = _v0.b;
		var decoder = _v0.c;
		return A3(
			$elm$browser$Browser$Events$MySub,
			node,
			name,
			A2($elm$json$Json$Decode$map, func, decoder));
	});
_Platform_effectManagers['Browser.Events'] = _Platform_createManager($elm$browser$Browser$Events$init, $elm$browser$Browser$Events$onEffects, $elm$browser$Browser$Events$onSelfMsg, 0, $elm$browser$Browser$Events$subMap);
var $elm$browser$Browser$Events$subscription = _Platform_leaf('Browser.Events');
var $elm$browser$Browser$Events$on = F3(
	function (node, name, decoder) {
		return $elm$browser$Browser$Events$subscription(
			A3($elm$browser$Browser$Events$MySub, node, name, decoder));
	});
var $elm$browser$Browser$Events$onResize = function (func) {
	return A3(
		$elm$browser$Browser$Events$on,
		1,
		'resize',
		A2(
			$elm$json$Json$Decode$field,
			'target',
			A3(
				$elm$json$Json$Decode$map2,
				func,
				A2($elm$json$Json$Decode$field, 'innerWidth', $elm$json$Json$Decode$int),
				A2($elm$json$Json$Decode$field, 'innerHeight', $elm$json$Json$Decode$int))));
};
var $author$project$Main$subscriptions = function (model) {
	return $elm$browser$Browser$Events$onResize(
		F2(
			function (w, h) {
				return $author$project$Main$WindowResized(
					{aG: h, al: w});
			}));
};
var $author$project$Main$JumbledWord = function (a) {
	return {$: 4, a: a};
};
var $author$project$Main$NextWord = function (a) {
	return {$: 3, a: a};
};
var $author$project$Main$incrementScore = function (model) {
	return model.W ? model.Y : (model.Y + 1);
};
var $elm$core$String$toLower = _String_toLower;
var $author$project$Main$match = F2(
	function (s1, s2) {
		return _Utils_eq(
			$elm$core$String$toLower(s1),
			$elm$core$String$toLower(s2));
	});
var $author$project$Main$checkMatch = function (model) {
	return A2($author$project$Main$match, model.ab, model.V) ? _Utils_Tuple2(
		_Utils_update(
			model,
			{
				W: true,
				w: 'Correct!',
				Y: $author$project$Main$incrementScore(model)
			}),
		$elm$core$Platform$Cmd$none) : _Utils_Tuple2(
		_Utils_update(
			model,
			{w: 'Wrong! Try again!'}),
		$elm$core$Platform$Cmd$none);
};
var $elm$core$List$append = F2(
	function (xs, ys) {
		if (!ys.b) {
			return xs;
		} else {
			return A3($elm$core$List$foldr, $elm$core$List$cons, ys, xs);
		}
	});
var $elm$random$Random$Generator = $elm$core$Basics$identity;
var $elm$random$Random$constant = function (value) {
	return function (seed) {
		return _Utils_Tuple2(value, seed);
	};
};
var $elm$core$List$drop = F2(
	function (n, list) {
		drop:
		while (true) {
			if (n <= 0) {
				return list;
			} else {
				if (!list.b) {
					return list;
				} else {
					var x = list.a;
					var xs = list.b;
					var $temp$n = n - 1,
						$temp$list = xs;
					n = $temp$n;
					list = $temp$list;
					continue drop;
				}
			}
		}
	});
var $elm$core$List$head = function (list) {
	if (list.b) {
		var x = list.a;
		var xs = list.b;
		return $elm$core$Maybe$Just(x);
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $elm_community$random_extra$Random$List$get = F2(
	function (index, list) {
		return $elm$core$List$head(
			A2($elm$core$List$drop, index, list));
	});
var $elm$core$Bitwise$and = _Bitwise_and;
var $elm$core$Basics$negate = function (n) {
	return -n;
};
var $elm$random$Random$Seed = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $elm$core$Bitwise$shiftRightZfBy = _Bitwise_shiftRightZfBy;
var $elm$random$Random$next = function (_v0) {
	var state0 = _v0.a;
	var incr = _v0.b;
	return A2($elm$random$Random$Seed, ((state0 * 1664525) + incr) >>> 0, incr);
};
var $elm$core$Bitwise$xor = _Bitwise_xor;
var $elm$random$Random$peel = function (_v0) {
	var state = _v0.a;
	var word = (state ^ (state >>> ((state >>> 28) + 4))) * 277803737;
	return ((word >>> 22) ^ word) >>> 0;
};
var $elm$random$Random$int = F2(
	function (a, b) {
		return function (seed0) {
			var _v0 = (_Utils_cmp(a, b) < 0) ? _Utils_Tuple2(a, b) : _Utils_Tuple2(b, a);
			var lo = _v0.a;
			var hi = _v0.b;
			var range = (hi - lo) + 1;
			if (!((range - 1) & range)) {
				return _Utils_Tuple2(
					(((range - 1) & $elm$random$Random$peel(seed0)) >>> 0) + lo,
					$elm$random$Random$next(seed0));
			} else {
				var threshhold = (((-range) >>> 0) % range) >>> 0;
				var accountForBias = function (seed) {
					accountForBias:
					while (true) {
						var x = $elm$random$Random$peel(seed);
						var seedN = $elm$random$Random$next(seed);
						if (_Utils_cmp(x, threshhold) < 0) {
							var $temp$seed = seedN;
							seed = $temp$seed;
							continue accountForBias;
						} else {
							return _Utils_Tuple2((x % range) + lo, seedN);
						}
					}
				};
				return accountForBias(seed0);
			}
		};
	});
var $elm$core$List$isEmpty = function (xs) {
	if (!xs.b) {
		return true;
	} else {
		return false;
	}
};
var $elm$random$Random$map = F2(
	function (func, _v0) {
		var genA = _v0;
		return function (seed0) {
			var _v1 = genA(seed0);
			var a = _v1.a;
			var seed1 = _v1.b;
			return _Utils_Tuple2(
				func(a),
				seed1);
		};
	});
var $elm$core$List$takeReverse = F3(
	function (n, list, kept) {
		takeReverse:
		while (true) {
			if (n <= 0) {
				return kept;
			} else {
				if (!list.b) {
					return kept;
				} else {
					var x = list.a;
					var xs = list.b;
					var $temp$n = n - 1,
						$temp$list = xs,
						$temp$kept = A2($elm$core$List$cons, x, kept);
					n = $temp$n;
					list = $temp$list;
					kept = $temp$kept;
					continue takeReverse;
				}
			}
		}
	});
var $elm$core$List$takeTailRec = F2(
	function (n, list) {
		return $elm$core$List$reverse(
			A3($elm$core$List$takeReverse, n, list, _List_Nil));
	});
var $elm$core$List$takeFast = F3(
	function (ctr, n, list) {
		if (n <= 0) {
			return _List_Nil;
		} else {
			var _v0 = _Utils_Tuple2(n, list);
			_v0$1:
			while (true) {
				_v0$5:
				while (true) {
					if (!_v0.b.b) {
						return list;
					} else {
						if (_v0.b.b.b) {
							switch (_v0.a) {
								case 1:
									break _v0$1;
								case 2:
									var _v2 = _v0.b;
									var x = _v2.a;
									var _v3 = _v2.b;
									var y = _v3.a;
									return _List_fromArray(
										[x, y]);
								case 3:
									if (_v0.b.b.b.b) {
										var _v4 = _v0.b;
										var x = _v4.a;
										var _v5 = _v4.b;
										var y = _v5.a;
										var _v6 = _v5.b;
										var z = _v6.a;
										return _List_fromArray(
											[x, y, z]);
									} else {
										break _v0$5;
									}
								default:
									if (_v0.b.b.b.b && _v0.b.b.b.b.b) {
										var _v7 = _v0.b;
										var x = _v7.a;
										var _v8 = _v7.b;
										var y = _v8.a;
										var _v9 = _v8.b;
										var z = _v9.a;
										var _v10 = _v9.b;
										var w = _v10.a;
										var tl = _v10.b;
										return (ctr > 1000) ? A2(
											$elm$core$List$cons,
											x,
											A2(
												$elm$core$List$cons,
												y,
												A2(
													$elm$core$List$cons,
													z,
													A2(
														$elm$core$List$cons,
														w,
														A2($elm$core$List$takeTailRec, n - 4, tl))))) : A2(
											$elm$core$List$cons,
											x,
											A2(
												$elm$core$List$cons,
												y,
												A2(
													$elm$core$List$cons,
													z,
													A2(
														$elm$core$List$cons,
														w,
														A3($elm$core$List$takeFast, ctr + 1, n - 4, tl)))));
									} else {
										break _v0$5;
									}
							}
						} else {
							if (_v0.a === 1) {
								break _v0$1;
							} else {
								break _v0$5;
							}
						}
					}
				}
				return list;
			}
			var _v1 = _v0.b;
			var x = _v1.a;
			return _List_fromArray(
				[x]);
		}
	});
var $elm$core$List$take = F2(
	function (n, list) {
		return A3($elm$core$List$takeFast, 0, n, list);
	});
var $elm_community$random_extra$Random$List$choose = function (list) {
	if ($elm$core$List$isEmpty(list)) {
		return $elm$random$Random$constant(
			_Utils_Tuple2($elm$core$Maybe$Nothing, list));
	} else {
		var lastIndex = $elm$core$List$length(list) - 1;
		var gen = A2($elm$random$Random$int, 0, lastIndex);
		var front = function (i) {
			return A2($elm$core$List$take, i, list);
		};
		var back = function (i) {
			return A2($elm$core$List$drop, i + 1, list);
		};
		return A2(
			$elm$random$Random$map,
			function (index) {
				return _Utils_Tuple2(
					A2($elm_community$random_extra$Random$List$get, index, list),
					A2(
						$elm$core$List$append,
						front(index),
						back(index)));
			},
			gen);
	}
};
var $elm$core$String$fromList = _String_fromList;
var $elm$random$Random$Generate = $elm$core$Basics$identity;
var $elm$random$Random$initialSeed = function (x) {
	var _v0 = $elm$random$Random$next(
		A2($elm$random$Random$Seed, 0, 1013904223));
	var state1 = _v0.a;
	var incr = _v0.b;
	var state2 = (state1 + x) >>> 0;
	return $elm$random$Random$next(
		A2($elm$random$Random$Seed, state2, incr));
};
var $elm$time$Time$Name = function (a) {
	return {$: 0, a: a};
};
var $elm$time$Time$Offset = function (a) {
	return {$: 1, a: a};
};
var $elm$time$Time$Zone = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $elm$time$Time$customZone = $elm$time$Time$Zone;
var $elm$time$Time$Posix = $elm$core$Basics$identity;
var $elm$time$Time$millisToPosix = $elm$core$Basics$identity;
var $elm$time$Time$now = _Time_now($elm$time$Time$millisToPosix);
var $elm$time$Time$posixToMillis = function (_v0) {
	var millis = _v0;
	return millis;
};
var $elm$random$Random$init = A2(
	$elm$core$Task$andThen,
	function (time) {
		return $elm$core$Task$succeed(
			$elm$random$Random$initialSeed(
				$elm$time$Time$posixToMillis(time)));
	},
	$elm$time$Time$now);
var $elm$random$Random$step = F2(
	function (_v0, seed) {
		var generator = _v0;
		return generator(seed);
	});
var $elm$random$Random$onEffects = F3(
	function (router, commands, seed) {
		if (!commands.b) {
			return $elm$core$Task$succeed(seed);
		} else {
			var generator = commands.a;
			var rest = commands.b;
			var _v1 = A2($elm$random$Random$step, generator, seed);
			var value = _v1.a;
			var newSeed = _v1.b;
			return A2(
				$elm$core$Task$andThen,
				function (_v2) {
					return A3($elm$random$Random$onEffects, router, rest, newSeed);
				},
				A2($elm$core$Platform$sendToApp, router, value));
		}
	});
var $elm$random$Random$onSelfMsg = F3(
	function (_v0, _v1, seed) {
		return $elm$core$Task$succeed(seed);
	});
var $elm$random$Random$cmdMap = F2(
	function (func, _v0) {
		var generator = _v0;
		return A2($elm$random$Random$map, func, generator);
	});
_Platform_effectManagers['Random'] = _Platform_createManager($elm$random$Random$init, $elm$random$Random$onEffects, $elm$random$Random$onSelfMsg, $elm$random$Random$cmdMap);
var $elm$random$Random$command = _Platform_leaf('Random');
var $elm$random$Random$generate = F2(
	function (tagger, generator) {
		return $elm$random$Random$command(
			A2($elm$random$Random$map, tagger, generator));
	});
var $elm$core$Array$fromListHelp = F3(
	function (list, nodeList, nodeListSize) {
		fromListHelp:
		while (true) {
			var _v0 = A2($elm$core$Elm$JsArray$initializeFromList, $elm$core$Array$branchFactor, list);
			var jsArray = _v0.a;
			var remainingItems = _v0.b;
			if (_Utils_cmp(
				$elm$core$Elm$JsArray$length(jsArray),
				$elm$core$Array$branchFactor) < 0) {
				return A2(
					$elm$core$Array$builderToArray,
					true,
					{f: nodeList, c: nodeListSize, e: jsArray});
			} else {
				var $temp$list = remainingItems,
					$temp$nodeList = A2(
					$elm$core$List$cons,
					$elm$core$Array$Leaf(jsArray),
					nodeList),
					$temp$nodeListSize = nodeListSize + 1;
				list = $temp$list;
				nodeList = $temp$nodeList;
				nodeListSize = $temp$nodeListSize;
				continue fromListHelp;
			}
		}
	});
var $elm$core$Array$fromList = function (list) {
	if (!list.b) {
		return $elm$core$Array$empty;
	} else {
		return A3($elm$core$Array$fromListHelp, list, _List_Nil, 0);
	}
};
var $elm$core$Array$length = function (_v0) {
	var len = _v0.a;
	return len;
};
var $elm$random$Random$listHelp = F4(
	function (revList, n, gen, seed) {
		listHelp:
		while (true) {
			if (n < 1) {
				return _Utils_Tuple2(revList, seed);
			} else {
				var _v0 = gen(seed);
				var value = _v0.a;
				var newSeed = _v0.b;
				var $temp$revList = A2($elm$core$List$cons, value, revList),
					$temp$n = n - 1,
					$temp$gen = gen,
					$temp$seed = newSeed;
				revList = $temp$revList;
				n = $temp$n;
				gen = $temp$gen;
				seed = $temp$seed;
				continue listHelp;
			}
		}
	});
var $elm$random$Random$list = F2(
	function (n, _v0) {
		var gen = _v0;
		return function (seed) {
			return A4($elm$random$Random$listHelp, _List_Nil, n, gen, seed);
		};
	});
var $elm$core$Dict$get = F2(
	function (targetKey, dict) {
		get:
		while (true) {
			if (dict.$ === -2) {
				return $elm$core$Maybe$Nothing;
			} else {
				var key = dict.b;
				var value = dict.c;
				var left = dict.d;
				var right = dict.e;
				var _v1 = A2($elm$core$Basics$compare, targetKey, key);
				switch (_v1) {
					case 0:
						var $temp$targetKey = targetKey,
							$temp$dict = left;
						targetKey = $temp$targetKey;
						dict = $temp$dict;
						continue get;
					case 1:
						return $elm$core$Maybe$Just(value);
					default:
						var $temp$targetKey = targetKey,
							$temp$dict = right;
						targetKey = $temp$targetKey;
						dict = $temp$dict;
						continue get;
				}
			}
		}
	});
var $owanturist$elm_union_find$UnionFind$findFast = F2(
	function (id, dict) {
		findFast:
		while (true) {
			var _v0 = A2($elm$core$Dict$get, id, dict);
			if (_v0.$ === 1) {
				return id;
			} else {
				var cursor = _v0.a;
				if (_Utils_eq(id, cursor)) {
					return id;
				} else {
					var $temp$id = cursor,
						$temp$dict = dict;
					id = $temp$id;
					dict = $temp$dict;
					continue findFast;
				}
			}
		}
	});
var $owanturist$elm_union_find$UnionFind$find = F2(
	function (id, _v0) {
		var dict = _v0.b;
		return A2($owanturist$elm_union_find$UnionFind$findFast, id, dict);
	});
var $elm$core$Array$bitMask = 4294967295 >>> (32 - $elm$core$Array$shiftStep);
var $elm$core$Basics$ge = _Utils_ge;
var $elm$core$Elm$JsArray$unsafeGet = _JsArray_unsafeGet;
var $elm$core$Array$getHelp = F3(
	function (shift, index, tree) {
		getHelp:
		while (true) {
			var pos = $elm$core$Array$bitMask & (index >>> shift);
			var _v0 = A2($elm$core$Elm$JsArray$unsafeGet, pos, tree);
			if (!_v0.$) {
				var subTree = _v0.a;
				var $temp$shift = shift - $elm$core$Array$shiftStep,
					$temp$index = index,
					$temp$tree = subTree;
				shift = $temp$shift;
				index = $temp$index;
				tree = $temp$tree;
				continue getHelp;
			} else {
				var values = _v0.a;
				return A2($elm$core$Elm$JsArray$unsafeGet, $elm$core$Array$bitMask & index, values);
			}
		}
	});
var $elm$core$Bitwise$shiftLeftBy = _Bitwise_shiftLeftBy;
var $elm$core$Array$tailIndex = function (len) {
	return (len >>> 5) << 5;
};
var $elm$core$Array$get = F2(
	function (index, _v0) {
		var len = _v0.a;
		var startShift = _v0.b;
		var tree = _v0.c;
		var tail = _v0.d;
		return ((index < 0) || (_Utils_cmp(index, len) > -1)) ? $elm$core$Maybe$Nothing : ((_Utils_cmp(
			index,
			$elm$core$Array$tailIndex(len)) > -1) ? $elm$core$Maybe$Just(
			A2($elm$core$Elm$JsArray$unsafeGet, $elm$core$Array$bitMask & index, tail)) : $elm$core$Maybe$Just(
			A3($elm$core$Array$getHelp, startShift, index, tree)));
	});
var $elm$core$Array$isEmpty = function (_v0) {
	var len = _v0.a;
	return !len;
};
var $elm$core$Basics$modBy = _Basics_modBy;
var $owanturist$elm_union_find$UnionFind$QuickUnionPathCompression = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $owanturist$elm_union_find$UnionFind$quickUnionPathCompression = A2($owanturist$elm_union_find$UnionFind$QuickUnionPathCompression, 0, $elm$core$Dict$empty);
var $elm$core$Tuple$second = function (_v0) {
	var y = _v0.b;
	return y;
};
var $owanturist$elm_union_find$UnionFind$findCompressed = F2(
	function (id, dict) {
		var _v0 = A2($elm$core$Dict$get, id, dict);
		if (_v0.$ === 1) {
			return _Utils_Tuple2(
				id,
				A3($elm$core$Dict$insert, id, id, dict));
		} else {
			var cursor = _v0.a;
			if (_Utils_eq(id, cursor)) {
				return _Utils_Tuple2(id, dict);
			} else {
				var _v1 = A2($owanturist$elm_union_find$UnionFind$findCompressed, cursor, dict);
				var parent = _v1.a;
				var nextDict = _v1.b;
				return _Utils_Tuple2(
					parent,
					A3($elm$core$Dict$insert, id, parent, nextDict));
			}
		}
	});
var $owanturist$elm_union_find$UnionFind$union = F3(
	function (left, right, _v0) {
		var count_ = _v0.a;
		var dict = _v0.b;
		var _v1 = A2($owanturist$elm_union_find$UnionFind$findCompressed, left, dict);
		var leftRoot = _v1.a;
		var leftDict = _v1.b;
		var _v2 = A2($owanturist$elm_union_find$UnionFind$findCompressed, right, leftDict);
		var rightRoot = _v2.a;
		var rightDict = _v2.b;
		return _Utils_eq(leftRoot, rightRoot) ? A2($owanturist$elm_union_find$UnionFind$QuickUnionPathCompression, count_, rightDict) : A2(
			$owanturist$elm_union_find$UnionFind$QuickUnionPathCompression,
			count_ + 1,
			A3($elm$core$Dict$insert, leftRoot, rightRoot, rightDict));
	});
var $elm_community$random_extra$Utils$selectUniqByIndexes = F2(
	function (values, randomIndexes) {
		var modByLength = $elm$core$Basics$modBy(
			$elm$core$Array$length(values));
		var step = F2(
			function (randomIndex, _v1) {
				var uf = _v1.a;
				var acc = _v1.b;
				var leaderOfElement = A2($owanturist$elm_union_find$UnionFind$find, randomIndex, uf);
				var leaderOfNextElement = A2(
					$owanturist$elm_union_find$UnionFind$find,
					modByLength(leaderOfElement + 1),
					uf);
				var _v0 = A2($elm$core$Array$get, leaderOfElement, values);
				if (_v0.$ === 1) {
					return _Utils_Tuple2(uf, acc);
				} else {
					var value = _v0.a;
					return _Utils_Tuple2(
						A3($owanturist$elm_union_find$UnionFind$union, leaderOfElement, leaderOfNextElement, uf),
						A2($elm$core$List$cons, value, acc));
				}
			});
		return $elm$core$Array$isEmpty(values) ? _List_Nil : A3(
			$elm$core$List$foldr,
			step,
			_Utils_Tuple2($owanturist$elm_union_find$UnionFind$quickUnionPathCompression, _List_Nil),
			randomIndexes).b;
	});
var $elm_community$random_extra$Random$List$shuffle = function (list) {
	var values = $elm$core$Array$fromList(list);
	var length = $elm$core$Array$length(values);
	return A2(
		$elm$random$Random$map,
		$elm_community$random_extra$Utils$selectUniqByIndexes(values),
		A2(
			$elm$random$Random$list,
			length,
			A2($elm$random$Random$int, 0, length - 1)));
};
var $elm$core$String$foldr = _String_foldr;
var $elm$core$String$toList = function (string) {
	return A3($elm$core$String$foldr, $elm$core$List$cons, _List_Nil, string);
};
var $author$project$Main$update = F2(
	function (msg, model) {
		switch (msg.$) {
			case 0:
				var newGuess = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{V: newGuess, w: ''}),
					$elm$core$Platform$Cmd$none);
			case 1:
				return $author$project$Main$checkMatch(model);
			case 2:
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{V: '', W: false, w: '', ab: ''}),
					A2(
						$elm$random$Random$generate,
						$author$project$Main$NextWord,
						$elm_community$random_extra$Random$List$choose(model.ay)));
			case 3:
				var _v1 = msg.a;
				var maybeWord = _v1.a;
				var wordsRemaining = _v1.b;
				if (!maybeWord.$) {
					var word = maybeWord.a;
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{ab: word, ay: wordsRemaining}),
						A2(
							$elm$random$Random$generate,
							$author$project$Main$JumbledWord,
							$elm_community$random_extra$Random$List$shuffle(
								$elm$core$String$toList(word))));
				} else {
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{w: 'No words left :('}),
						$elm$core$Platform$Cmd$none);
				}
			case 4:
				var jumbledChars = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							au: $elm$core$String$fromList(jumbledChars)
						}),
					$elm$core$Platform$Cmd$none);
			case 5:
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{W: true, w: 'Stupid! The word was ' + model.ab}),
					$elm$core$Platform$Cmd$none);
			default:
				var newDim = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{T: newDim}),
					$elm$core$Platform$Cmd$none);
		}
	});
var $elm$json$Json$Decode$value = _Json_decodeValue;
var $author$project$Main$GuessChanged = function (a) {
	return {$: 0, a: a};
};
var $author$project$Main$baseFontSize = 18;
var $author$project$Main$basePadding = {b2: 0, bb: 15, bt: 5, dm: 30};
var $author$project$Main$baseSpacing = 12;
var $author$project$Main$Check = {$: 1};
var $author$project$Main$GiveUp = {$: 5};
var $author$project$Main$Next = {$: 2};
var $mdgriffith$elm_ui$Internal$Model$Attr = function (a) {
	return {$: 1, a: a};
};
var $mdgriffith$elm_ui$Internal$Model$Button = {$: 8};
var $mdgriffith$elm_ui$Internal$Model$Describe = function (a) {
	return {$: 2, a: a};
};
var $mdgriffith$elm_ui$Internal$Model$Unkeyed = function (a) {
	return {$: 0, a: a};
};
var $mdgriffith$elm_ui$Internal$Model$AsEl = 2;
var $mdgriffith$elm_ui$Internal$Model$asEl = 2;
var $mdgriffith$elm_ui$Internal$Style$classes = {bH: 'a', az: 'atv', bJ: 'ab', bK: 'cx', bL: 'cy', bM: 'acb', bN: 'accx', bO: 'accy', bP: 'acr', aW: 'al', aX: 'ar', bQ: 'at', aA: 'ah', aB: 'av', bS: 's', bW: 'bh', bX: 'b', bZ: 'w7', b$: 'bd', b0: 'bdt', am: 'bn', b1: 'bs', an: 'cpe', b8: 'cp', b9: 'cpx', ca: 'cpy', B: 'c', ap: 'ctr', aq: 'cb', ar: 'ccx', C: 'ccy', ae: 'cl', as: 'cr', cc: 'ct', cd: 'cptr', ce: 'ctxt', cl: 'fcs', a1: 'focus-within', cm: 'fs', cn: 'g', aF: 'hbh', aH: 'hc', a4: 'he', aI: 'hf', a5: 'hfp', cp: 'hv', cr: 'ic', ct: 'fr', cv: 'iml', cw: 'imlf', cx: 'imlp', cy: 'implw', cz: 'it', cA: 'i', bc: 'lnk', X: 'nb', bf: 'notxt', cF: 'ol', cH: 'or', M: 'oq', cM: 'oh', bi: 'pg', bj: 'p', cN: 'ppe', cQ: 'ui', s: 'r', cS: 'sb', cT: 'sbx', cU: 'sby', cV: 'sbt', cX: 'e', cY: 'cap', cZ: 'sev', c3: 'sk', c6: 't', c7: 'tc', c8: 'w8', c9: 'w2', da: 'w9', db: 'tj', aw: 'tja', dc: 'tl', dd: 'w3', de: 'w5', df: 'w4', dg: 'tr', dh: 'w6', di: 'w1', dj: 'tun', by: 'ts', P: 'clr', dp: 'u', aS: 'wc', bC: 'we', aT: 'wf', bD: 'wfp', aU: 'wrp'};
var $elm$json$Json$Encode$bool = _Json_wrap;
var $elm$html$Html$Attributes$boolProperty = F2(
	function (key, bool) {
		return A2(
			_VirtualDom_property,
			key,
			$elm$json$Json$Encode$bool(bool));
	});
var $elm$html$Html$Attributes$disabled = $elm$html$Html$Attributes$boolProperty('disabled');
var $mdgriffith$elm_ui$Internal$Model$Generic = {$: 0};
var $mdgriffith$elm_ui$Internal$Model$div = $mdgriffith$elm_ui$Internal$Model$Generic;
var $mdgriffith$elm_ui$Internal$Model$NoNearbyChildren = {$: 0};
var $mdgriffith$elm_ui$Internal$Model$columnClass = $mdgriffith$elm_ui$Internal$Style$classes.bS + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.B);
var $mdgriffith$elm_ui$Internal$Model$gridClass = $mdgriffith$elm_ui$Internal$Style$classes.bS + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.cn);
var $mdgriffith$elm_ui$Internal$Model$pageClass = $mdgriffith$elm_ui$Internal$Style$classes.bS + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.bi);
var $mdgriffith$elm_ui$Internal$Model$paragraphClass = $mdgriffith$elm_ui$Internal$Style$classes.bS + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.bj);
var $mdgriffith$elm_ui$Internal$Model$rowClass = $mdgriffith$elm_ui$Internal$Style$classes.bS + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.s);
var $mdgriffith$elm_ui$Internal$Model$singleClass = $mdgriffith$elm_ui$Internal$Style$classes.bS + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.cX);
var $mdgriffith$elm_ui$Internal$Model$contextClasses = function (context) {
	switch (context) {
		case 0:
			return $mdgriffith$elm_ui$Internal$Model$rowClass;
		case 1:
			return $mdgriffith$elm_ui$Internal$Model$columnClass;
		case 2:
			return $mdgriffith$elm_ui$Internal$Model$singleClass;
		case 3:
			return $mdgriffith$elm_ui$Internal$Model$gridClass;
		case 4:
			return $mdgriffith$elm_ui$Internal$Model$paragraphClass;
		default:
			return $mdgriffith$elm_ui$Internal$Model$pageClass;
	}
};
var $mdgriffith$elm_ui$Internal$Model$Keyed = function (a) {
	return {$: 1, a: a};
};
var $mdgriffith$elm_ui$Internal$Model$NoStyleSheet = {$: 0};
var $mdgriffith$elm_ui$Internal$Model$Styled = function (a) {
	return {$: 1, a: a};
};
var $mdgriffith$elm_ui$Internal$Model$Unstyled = function (a) {
	return {$: 0, a: a};
};
var $mdgriffith$elm_ui$Internal$Model$addChildren = F2(
	function (existing, nearbyChildren) {
		switch (nearbyChildren.$) {
			case 0:
				return existing;
			case 1:
				var behind = nearbyChildren.a;
				return _Utils_ap(behind, existing);
			case 2:
				var inFront = nearbyChildren.a;
				return _Utils_ap(existing, inFront);
			default:
				var behind = nearbyChildren.a;
				var inFront = nearbyChildren.b;
				return _Utils_ap(
					behind,
					_Utils_ap(existing, inFront));
		}
	});
var $mdgriffith$elm_ui$Internal$Model$addKeyedChildren = F3(
	function (key, existing, nearbyChildren) {
		switch (nearbyChildren.$) {
			case 0:
				return existing;
			case 1:
				var behind = nearbyChildren.a;
				return _Utils_ap(
					A2(
						$elm$core$List$map,
						function (x) {
							return _Utils_Tuple2(key, x);
						},
						behind),
					existing);
			case 2:
				var inFront = nearbyChildren.a;
				return _Utils_ap(
					existing,
					A2(
						$elm$core$List$map,
						function (x) {
							return _Utils_Tuple2(key, x);
						},
						inFront));
			default:
				var behind = nearbyChildren.a;
				var inFront = nearbyChildren.b;
				return _Utils_ap(
					A2(
						$elm$core$List$map,
						function (x) {
							return _Utils_Tuple2(key, x);
						},
						behind),
					_Utils_ap(
						existing,
						A2(
							$elm$core$List$map,
							function (x) {
								return _Utils_Tuple2(key, x);
							},
							inFront)));
		}
	});
var $mdgriffith$elm_ui$Internal$Model$AsParagraph = 4;
var $mdgriffith$elm_ui$Internal$Model$asParagraph = 4;
var $mdgriffith$elm_ui$Internal$Flag$Flag = function (a) {
	return {$: 0, a: a};
};
var $mdgriffith$elm_ui$Internal$Flag$Second = function (a) {
	return {$: 1, a: a};
};
var $mdgriffith$elm_ui$Internal$Flag$flag = function (i) {
	return (i > 31) ? $mdgriffith$elm_ui$Internal$Flag$Second(1 << (i - 32)) : $mdgriffith$elm_ui$Internal$Flag$Flag(1 << i);
};
var $mdgriffith$elm_ui$Internal$Flag$alignBottom = $mdgriffith$elm_ui$Internal$Flag$flag(41);
var $mdgriffith$elm_ui$Internal$Flag$alignRight = $mdgriffith$elm_ui$Internal$Flag$flag(40);
var $mdgriffith$elm_ui$Internal$Flag$centerX = $mdgriffith$elm_ui$Internal$Flag$flag(42);
var $mdgriffith$elm_ui$Internal$Flag$centerY = $mdgriffith$elm_ui$Internal$Flag$flag(43);
var $elm$json$Json$Encode$string = _Json_wrap;
var $elm$html$Html$Attributes$stringProperty = F2(
	function (key, string) {
		return A2(
			_VirtualDom_property,
			key,
			$elm$json$Json$Encode$string(string));
	});
var $elm$html$Html$Attributes$class = $elm$html$Html$Attributes$stringProperty('className');
var $elm$html$Html$div = _VirtualDom_node('div');
var $elm$core$Set$Set_elm_builtin = $elm$core$Basics$identity;
var $elm$core$Set$empty = $elm$core$Dict$empty;
var $mdgriffith$elm_ui$Internal$Model$lengthClassName = function (x) {
	switch (x.$) {
		case 0:
			var px = x.a;
			return $elm$core$String$fromInt(px) + 'px';
		case 1:
			return 'auto';
		case 2:
			var i = x.a;
			return $elm$core$String$fromInt(i) + 'fr';
		case 3:
			var min = x.a;
			var len = x.b;
			return 'min' + ($elm$core$String$fromInt(min) + $mdgriffith$elm_ui$Internal$Model$lengthClassName(len));
		default:
			var max = x.a;
			var len = x.b;
			return 'max' + ($elm$core$String$fromInt(max) + $mdgriffith$elm_ui$Internal$Model$lengthClassName(len));
	}
};
var $elm$core$Basics$round = _Basics_round;
var $mdgriffith$elm_ui$Internal$Model$floatClass = function (x) {
	return $elm$core$String$fromInt(
		$elm$core$Basics$round(x * 255));
};
var $mdgriffith$elm_ui$Internal$Model$transformClass = function (transform) {
	switch (transform.$) {
		case 0:
			return $elm$core$Maybe$Nothing;
		case 1:
			var _v1 = transform.a;
			var x = _v1.a;
			var y = _v1.b;
			var z = _v1.c;
			return $elm$core$Maybe$Just(
				'mv-' + ($mdgriffith$elm_ui$Internal$Model$floatClass(x) + ('-' + ($mdgriffith$elm_ui$Internal$Model$floatClass(y) + ('-' + $mdgriffith$elm_ui$Internal$Model$floatClass(z))))));
		default:
			var _v2 = transform.a;
			var tx = _v2.a;
			var ty = _v2.b;
			var tz = _v2.c;
			var _v3 = transform.b;
			var sx = _v3.a;
			var sy = _v3.b;
			var sz = _v3.c;
			var _v4 = transform.c;
			var ox = _v4.a;
			var oy = _v4.b;
			var oz = _v4.c;
			var angle = transform.d;
			return $elm$core$Maybe$Just(
				'tfrm-' + ($mdgriffith$elm_ui$Internal$Model$floatClass(tx) + ('-' + ($mdgriffith$elm_ui$Internal$Model$floatClass(ty) + ('-' + ($mdgriffith$elm_ui$Internal$Model$floatClass(tz) + ('-' + ($mdgriffith$elm_ui$Internal$Model$floatClass(sx) + ('-' + ($mdgriffith$elm_ui$Internal$Model$floatClass(sy) + ('-' + ($mdgriffith$elm_ui$Internal$Model$floatClass(sz) + ('-' + ($mdgriffith$elm_ui$Internal$Model$floatClass(ox) + ('-' + ($mdgriffith$elm_ui$Internal$Model$floatClass(oy) + ('-' + ($mdgriffith$elm_ui$Internal$Model$floatClass(oz) + ('-' + $mdgriffith$elm_ui$Internal$Model$floatClass(angle))))))))))))))))))));
	}
};
var $elm$core$Maybe$withDefault = F2(
	function (_default, maybe) {
		if (!maybe.$) {
			var value = maybe.a;
			return value;
		} else {
			return _default;
		}
	});
var $mdgriffith$elm_ui$Internal$Model$getStyleName = function (style) {
	switch (style.$) {
		case 13:
			var name = style.a;
			return name;
		case 12:
			var name = style.a;
			var o = style.b;
			return name;
		case 0:
			var _class = style.a;
			return _class;
		case 1:
			var name = style.a;
			return name;
		case 2:
			var i = style.a;
			return 'font-size-' + $elm$core$String$fromInt(i);
		case 3:
			var _class = style.a;
			return _class;
		case 4:
			var _class = style.a;
			return _class;
		case 5:
			var cls = style.a;
			var x = style.b;
			var y = style.c;
			return cls;
		case 7:
			var cls = style.a;
			var top = style.b;
			var right = style.c;
			var bottom = style.d;
			var left = style.e;
			return cls;
		case 6:
			var cls = style.a;
			var top = style.b;
			var right = style.c;
			var bottom = style.d;
			var left = style.e;
			return cls;
		case 8:
			var template = style.a;
			return 'grid-rows-' + (A2(
				$elm$core$String$join,
				'-',
				A2($elm$core$List$map, $mdgriffith$elm_ui$Internal$Model$lengthClassName, template.cR)) + ('-cols-' + (A2(
				$elm$core$String$join,
				'-',
				A2($elm$core$List$map, $mdgriffith$elm_ui$Internal$Model$lengthClassName, template.u)) + ('-space-x-' + ($mdgriffith$elm_ui$Internal$Model$lengthClassName(template.c_.a) + ('-space-y-' + $mdgriffith$elm_ui$Internal$Model$lengthClassName(template.c_.b)))))));
		case 9:
			var pos = style.a;
			return 'gp grid-pos-' + ($elm$core$String$fromInt(pos.s) + ('-' + ($elm$core$String$fromInt(pos.cb) + ('-' + ($elm$core$String$fromInt(pos.al) + ('-' + $elm$core$String$fromInt(pos.aG)))))));
		case 11:
			var selector = style.a;
			var subStyle = style.b;
			var name = function () {
				switch (selector) {
					case 0:
						return 'fs';
					case 1:
						return 'hv';
					default:
						return 'act';
				}
			}();
			return A2(
				$elm$core$String$join,
				' ',
				A2(
					$elm$core$List$map,
					function (sty) {
						var _v1 = $mdgriffith$elm_ui$Internal$Model$getStyleName(sty);
						if (_v1 === '') {
							return '';
						} else {
							var styleName = _v1;
							return styleName + ('-' + name);
						}
					},
					subStyle));
		default:
			var x = style.a;
			return A2(
				$elm$core$Maybe$withDefault,
				'',
				$mdgriffith$elm_ui$Internal$Model$transformClass(x));
	}
};
var $elm$core$Set$insert = F2(
	function (key, _v0) {
		var dict = _v0;
		return A3($elm$core$Dict$insert, key, 0, dict);
	});
var $elm$core$Dict$member = F2(
	function (key, dict) {
		var _v0 = A2($elm$core$Dict$get, key, dict);
		if (!_v0.$) {
			return true;
		} else {
			return false;
		}
	});
var $elm$core$Set$member = F2(
	function (key, _v0) {
		var dict = _v0;
		return A2($elm$core$Dict$member, key, dict);
	});
var $mdgriffith$elm_ui$Internal$Model$reduceStyles = F2(
	function (style, nevermind) {
		var cache = nevermind.a;
		var existing = nevermind.b;
		var styleName = $mdgriffith$elm_ui$Internal$Model$getStyleName(style);
		return A2($elm$core$Set$member, styleName, cache) ? nevermind : _Utils_Tuple2(
			A2($elm$core$Set$insert, styleName, cache),
			A2($elm$core$List$cons, style, existing));
	});
var $mdgriffith$elm_ui$Internal$Model$Property = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $mdgriffith$elm_ui$Internal$Model$Style = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $mdgriffith$elm_ui$Internal$Style$dot = function (c) {
	return '.' + c;
};
var $elm$core$String$fromFloat = _String_fromNumber;
var $mdgriffith$elm_ui$Internal$Model$formatColor = function (_v0) {
	var red = _v0.a;
	var green = _v0.b;
	var blue = _v0.c;
	var alpha = _v0.d;
	return 'rgba(' + ($elm$core$String$fromInt(
		$elm$core$Basics$round(red * 255)) + ((',' + $elm$core$String$fromInt(
		$elm$core$Basics$round(green * 255))) + ((',' + $elm$core$String$fromInt(
		$elm$core$Basics$round(blue * 255))) + (',' + ($elm$core$String$fromFloat(alpha) + ')')))));
};
var $mdgriffith$elm_ui$Internal$Model$formatBoxShadow = function (shadow) {
	return A2(
		$elm$core$String$join,
		' ',
		A2(
			$elm$core$List$filterMap,
			$elm$core$Basics$identity,
			_List_fromArray(
				[
					shadow.a8 ? $elm$core$Maybe$Just('inset') : $elm$core$Maybe$Nothing,
					$elm$core$Maybe$Just(
					$elm$core$String$fromFloat(shadow.bg.a) + 'px'),
					$elm$core$Maybe$Just(
					$elm$core$String$fromFloat(shadow.bg.b) + 'px'),
					$elm$core$Maybe$Just(
					$elm$core$String$fromFloat(shadow.R) + 'px'),
					$elm$core$Maybe$Just(
					$elm$core$String$fromFloat(shadow.Z) + 'px'),
					$elm$core$Maybe$Just(
					$mdgriffith$elm_ui$Internal$Model$formatColor(shadow.S))
				])));
};
var $elm$core$Maybe$map = F2(
	function (f, maybe) {
		if (!maybe.$) {
			var value = maybe.a;
			return $elm$core$Maybe$Just(
				f(value));
		} else {
			return $elm$core$Maybe$Nothing;
		}
	});
var $elm$core$Tuple$mapFirst = F2(
	function (func, _v0) {
		var x = _v0.a;
		var y = _v0.b;
		return _Utils_Tuple2(
			func(x),
			y);
	});
var $elm$core$Tuple$mapSecond = F2(
	function (func, _v0) {
		var x = _v0.a;
		var y = _v0.b;
		return _Utils_Tuple2(
			x,
			func(y));
	});
var $mdgriffith$elm_ui$Internal$Model$renderFocusStyle = function (focus) {
	return _List_fromArray(
		[
			A2(
			$mdgriffith$elm_ui$Internal$Model$Style,
			$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.a1) + ':focus-within',
			A2(
				$elm$core$List$filterMap,
				$elm$core$Basics$identity,
				_List_fromArray(
					[
						A2(
						$elm$core$Maybe$map,
						function (color) {
							return A2(
								$mdgriffith$elm_ui$Internal$Model$Property,
								'border-color',
								$mdgriffith$elm_ui$Internal$Model$formatColor(color));
						},
						focus.b_),
						A2(
						$elm$core$Maybe$map,
						function (color) {
							return A2(
								$mdgriffith$elm_ui$Internal$Model$Property,
								'background-color',
								$mdgriffith$elm_ui$Internal$Model$formatColor(color));
						},
						focus.bU),
						A2(
						$elm$core$Maybe$map,
						function (shadow) {
							return A2(
								$mdgriffith$elm_ui$Internal$Model$Property,
								'box-shadow',
								$mdgriffith$elm_ui$Internal$Model$formatBoxShadow(
									{
										R: shadow.R,
										S: shadow.S,
										a8: false,
										bg: A2(
											$elm$core$Tuple$mapSecond,
											$elm$core$Basics$toFloat,
											A2($elm$core$Tuple$mapFirst, $elm$core$Basics$toFloat, shadow.bg)),
										Z: shadow.Z
									}));
						},
						focus.cW),
						$elm$core$Maybe$Just(
						A2($mdgriffith$elm_ui$Internal$Model$Property, 'outline', 'none'))
					]))),
			A2(
			$mdgriffith$elm_ui$Internal$Model$Style,
			$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.bS) + (':focus .focusable, ' + ($mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.bS) + '.focusable:focus')),
			A2(
				$elm$core$List$filterMap,
				$elm$core$Basics$identity,
				_List_fromArray(
					[
						A2(
						$elm$core$Maybe$map,
						function (color) {
							return A2(
								$mdgriffith$elm_ui$Internal$Model$Property,
								'border-color',
								$mdgriffith$elm_ui$Internal$Model$formatColor(color));
						},
						focus.b_),
						A2(
						$elm$core$Maybe$map,
						function (color) {
							return A2(
								$mdgriffith$elm_ui$Internal$Model$Property,
								'background-color',
								$mdgriffith$elm_ui$Internal$Model$formatColor(color));
						},
						focus.bU),
						A2(
						$elm$core$Maybe$map,
						function (shadow) {
							return A2(
								$mdgriffith$elm_ui$Internal$Model$Property,
								'box-shadow',
								$mdgriffith$elm_ui$Internal$Model$formatBoxShadow(
									{
										R: shadow.R,
										S: shadow.S,
										a8: false,
										bg: A2(
											$elm$core$Tuple$mapSecond,
											$elm$core$Basics$toFloat,
											A2($elm$core$Tuple$mapFirst, $elm$core$Basics$toFloat, shadow.bg)),
										Z: shadow.Z
									}));
						},
						focus.cW),
						$elm$core$Maybe$Just(
						A2($mdgriffith$elm_ui$Internal$Model$Property, 'outline', 'none'))
					])))
		]);
};
var $elm$virtual_dom$VirtualDom$node = function (tag) {
	return _VirtualDom_node(
		_VirtualDom_noScript(tag));
};
var $elm$virtual_dom$VirtualDom$property = F2(
	function (key, value) {
		return A2(
			_VirtualDom_property,
			_VirtualDom_noInnerHtmlOrFormAction(key),
			_VirtualDom_noJavaScriptOrHtmlUri(value));
	});
var $mdgriffith$elm_ui$Internal$Style$Batch = function (a) {
	return {$: 5, a: a};
};
var $mdgriffith$elm_ui$Internal$Style$Child = F2(
	function (a, b) {
		return {$: 1, a: a, b: b};
	});
var $mdgriffith$elm_ui$Internal$Style$Class = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $mdgriffith$elm_ui$Internal$Style$Descriptor = F2(
	function (a, b) {
		return {$: 3, a: a, b: b};
	});
var $mdgriffith$elm_ui$Internal$Style$Left = 3;
var $mdgriffith$elm_ui$Internal$Style$Prop = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $mdgriffith$elm_ui$Internal$Style$Right = 2;
var $mdgriffith$elm_ui$Internal$Style$Self = $elm$core$Basics$identity;
var $mdgriffith$elm_ui$Internal$Style$Supports = F2(
	function (a, b) {
		return {$: 2, a: a, b: b};
	});
var $mdgriffith$elm_ui$Internal$Style$Content = $elm$core$Basics$identity;
var $mdgriffith$elm_ui$Internal$Style$Bottom = 1;
var $mdgriffith$elm_ui$Internal$Style$CenterX = 4;
var $mdgriffith$elm_ui$Internal$Style$CenterY = 5;
var $mdgriffith$elm_ui$Internal$Style$Top = 0;
var $mdgriffith$elm_ui$Internal$Style$alignments = _List_fromArray(
	[0, 1, 2, 3, 4, 5]);
var $elm$core$List$concat = function (lists) {
	return A3($elm$core$List$foldr, $elm$core$List$append, _List_Nil, lists);
};
var $elm$core$List$concatMap = F2(
	function (f, list) {
		return $elm$core$List$concat(
			A2($elm$core$List$map, f, list));
	});
var $mdgriffith$elm_ui$Internal$Style$contentName = function (desc) {
	switch (desc) {
		case 0:
			var _v1 = desc;
			return $mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.cc);
		case 1:
			var _v2 = desc;
			return $mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.aq);
		case 2:
			var _v3 = desc;
			return $mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.as);
		case 3:
			var _v4 = desc;
			return $mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.ae);
		case 4:
			var _v5 = desc;
			return $mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.ar);
		default:
			var _v6 = desc;
			return $mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.C);
	}
};
var $mdgriffith$elm_ui$Internal$Style$selfName = function (desc) {
	switch (desc) {
		case 0:
			var _v1 = desc;
			return $mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.bQ);
		case 1:
			var _v2 = desc;
			return $mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.bJ);
		case 2:
			var _v3 = desc;
			return $mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.aX);
		case 3:
			var _v4 = desc;
			return $mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.aW);
		case 4:
			var _v5 = desc;
			return $mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.bK);
		default:
			var _v6 = desc;
			return $mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.bL);
	}
};
var $mdgriffith$elm_ui$Internal$Style$describeAlignment = function (values) {
	var createDescription = function (alignment) {
		var _v0 = values(alignment);
		var content = _v0.a;
		var indiv = _v0.b;
		return _List_fromArray(
			[
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$contentName(alignment),
				content),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Child,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.bS),
				_List_fromArray(
					[
						A2(
						$mdgriffith$elm_ui$Internal$Style$Descriptor,
						$mdgriffith$elm_ui$Internal$Style$selfName(alignment),
						indiv)
					]))
			]);
	};
	return $mdgriffith$elm_ui$Internal$Style$Batch(
		A2($elm$core$List$concatMap, createDescription, $mdgriffith$elm_ui$Internal$Style$alignments));
};
var $mdgriffith$elm_ui$Internal$Style$elDescription = _List_fromArray(
	[
		A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'flex'),
		A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-direction', 'column'),
		A2($mdgriffith$elm_ui$Internal$Style$Prop, 'white-space', 'pre'),
		A2(
		$mdgriffith$elm_ui$Internal$Style$Descriptor,
		$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.aF),
		_List_fromArray(
			[
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'z-index', '0'),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Child,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.bW),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'z-index', '-1')
					]))
			])),
		A2(
		$mdgriffith$elm_ui$Internal$Style$Descriptor,
		$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.cV),
		_List_fromArray(
			[
				A2(
				$mdgriffith$elm_ui$Internal$Style$Child,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.c6),
				_List_fromArray(
					[
						A2(
						$mdgriffith$elm_ui$Internal$Style$Descriptor,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.aI),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '0')
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Descriptor,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.aT),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'auto !important')
							]))
					]))
			])),
		A2(
		$mdgriffith$elm_ui$Internal$Style$Child,
		$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.aH),
		_List_fromArray(
			[
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'height', 'auto')
			])),
		A2(
		$mdgriffith$elm_ui$Internal$Style$Child,
		$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.aI),
		_List_fromArray(
			[
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '100000')
			])),
		A2(
		$mdgriffith$elm_ui$Internal$Style$Child,
		$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.aT),
		_List_fromArray(
			[
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'width', '100%')
			])),
		A2(
		$mdgriffith$elm_ui$Internal$Style$Child,
		$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.bD),
		_List_fromArray(
			[
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'width', '100%')
			])),
		A2(
		$mdgriffith$elm_ui$Internal$Style$Child,
		$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.aS),
		_List_fromArray(
			[
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'flex-start')
			])),
		$mdgriffith$elm_ui$Internal$Style$describeAlignment(
		function (alignment) {
			switch (alignment) {
				case 0:
					return _Utils_Tuple2(
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'justify-content', 'flex-start')
							]),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin-bottom', 'auto !important'),
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin-top', '0 !important')
							]));
				case 1:
					return _Utils_Tuple2(
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'justify-content', 'flex-end')
							]),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin-top', 'auto !important'),
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin-bottom', '0 !important')
							]));
				case 2:
					return _Utils_Tuple2(
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-items', 'flex-end')
							]),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'flex-end')
							]));
				case 3:
					return _Utils_Tuple2(
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-items', 'flex-start')
							]),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'flex-start')
							]));
				case 4:
					return _Utils_Tuple2(
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-items', 'center')
							]),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'center')
							]));
				default:
					return _Utils_Tuple2(
						_List_fromArray(
							[
								A2(
								$mdgriffith$elm_ui$Internal$Style$Child,
								$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.bS),
								_List_fromArray(
									[
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin-top', 'auto'),
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin-bottom', 'auto')
									]))
							]),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin-top', 'auto !important'),
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin-bottom', 'auto !important')
							]));
			}
		})
	]);
var $mdgriffith$elm_ui$Internal$Style$gridAlignments = function (values) {
	var createDescription = function (alignment) {
		return _List_fromArray(
			[
				A2(
				$mdgriffith$elm_ui$Internal$Style$Child,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.bS),
				_List_fromArray(
					[
						A2(
						$mdgriffith$elm_ui$Internal$Style$Descriptor,
						$mdgriffith$elm_ui$Internal$Style$selfName(alignment),
						values(alignment))
					]))
			]);
	};
	return $mdgriffith$elm_ui$Internal$Style$Batch(
		A2($elm$core$List$concatMap, createDescription, $mdgriffith$elm_ui$Internal$Style$alignments));
};
var $mdgriffith$elm_ui$Internal$Style$Above = 0;
var $mdgriffith$elm_ui$Internal$Style$Behind = 5;
var $mdgriffith$elm_ui$Internal$Style$Below = 1;
var $mdgriffith$elm_ui$Internal$Style$OnLeft = 3;
var $mdgriffith$elm_ui$Internal$Style$OnRight = 2;
var $mdgriffith$elm_ui$Internal$Style$Within = 4;
var $mdgriffith$elm_ui$Internal$Style$locations = function () {
	var loc = 0;
	var _v0 = function () {
		switch (loc) {
			case 0:
				return 0;
			case 1:
				return 0;
			case 2:
				return 0;
			case 3:
				return 0;
			case 4:
				return 0;
			default:
				return 0;
		}
	}();
	return _List_fromArray(
		[0, 1, 2, 3, 4, 5]);
}();
var $mdgriffith$elm_ui$Internal$Style$baseSheet = _List_fromArray(
	[
		A2(
		$mdgriffith$elm_ui$Internal$Style$Class,
		'html,body',
		_List_fromArray(
			[
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'height', '100%'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'padding', '0'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin', '0')
			])),
		A2(
		$mdgriffith$elm_ui$Internal$Style$Class,
		_Utils_ap(
			$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.bS),
			_Utils_ap(
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.cX),
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.cr))),
		_List_fromArray(
			[
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'block')
			])),
		A2(
		$mdgriffith$elm_ui$Internal$Style$Class,
		$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.bS) + ':focus',
		_List_fromArray(
			[
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'outline', 'none')
			])),
		A2(
		$mdgriffith$elm_ui$Internal$Style$Class,
		$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.cQ),
		_List_fromArray(
			[
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'width', '100%'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'height', 'auto'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'min-height', '100%'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'z-index', '0'),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				_Utils_ap(
					$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.bS),
					$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.aI)),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'height', '100%'),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.aI),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'height', '100%')
							]))
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Child,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.ct),
				_List_fromArray(
					[
						A2(
						$mdgriffith$elm_ui$Internal$Style$Descriptor,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.X),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'position', 'fixed')
							]))
					]))
			])),
		A2(
		$mdgriffith$elm_ui$Internal$Style$Class,
		$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.X),
		_List_fromArray(
			[
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'position', 'relative'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'border', 'none'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'flex'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-direction', 'row'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-basis', 'auto'),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.cX),
				$mdgriffith$elm_ui$Internal$Style$elDescription),
				$mdgriffith$elm_ui$Internal$Style$Batch(
				function (fn) {
					return A2($elm$core$List$map, fn, $mdgriffith$elm_ui$Internal$Style$locations);
				}(
					function (loc) {
						switch (loc) {
							case 0:
								return A2(
									$mdgriffith$elm_ui$Internal$Style$Descriptor,
									$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.bH),
									_List_fromArray(
										[
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'position', 'absolute'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'bottom', '100%'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'left', '0'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'width', '100%'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'z-index', '20'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin', '0 !important'),
											A2(
											$mdgriffith$elm_ui$Internal$Style$Child,
											$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.aI),
											_List_fromArray(
												[
													A2($mdgriffith$elm_ui$Internal$Style$Prop, 'height', 'auto')
												])),
											A2(
											$mdgriffith$elm_ui$Internal$Style$Child,
											$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.aT),
											_List_fromArray(
												[
													A2($mdgriffith$elm_ui$Internal$Style$Prop, 'width', '100%')
												])),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'pointer-events', 'none'),
											A2(
											$mdgriffith$elm_ui$Internal$Style$Child,
											'*',
											_List_fromArray(
												[
													A2($mdgriffith$elm_ui$Internal$Style$Prop, 'pointer-events', 'auto')
												]))
										]));
							case 1:
								return A2(
									$mdgriffith$elm_ui$Internal$Style$Descriptor,
									$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.bX),
									_List_fromArray(
										[
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'position', 'absolute'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'bottom', '0'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'left', '0'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'height', '0'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'width', '100%'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'z-index', '20'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin', '0 !important'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'pointer-events', 'none'),
											A2(
											$mdgriffith$elm_ui$Internal$Style$Child,
											'*',
											_List_fromArray(
												[
													A2($mdgriffith$elm_ui$Internal$Style$Prop, 'pointer-events', 'auto')
												])),
											A2(
											$mdgriffith$elm_ui$Internal$Style$Child,
											$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.aI),
											_List_fromArray(
												[
													A2($mdgriffith$elm_ui$Internal$Style$Prop, 'height', 'auto')
												]))
										]));
							case 2:
								return A2(
									$mdgriffith$elm_ui$Internal$Style$Descriptor,
									$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.cH),
									_List_fromArray(
										[
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'position', 'absolute'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'left', '100%'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'top', '0'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'height', '100%'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin', '0 !important'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'z-index', '20'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'pointer-events', 'none'),
											A2(
											$mdgriffith$elm_ui$Internal$Style$Child,
											'*',
											_List_fromArray(
												[
													A2($mdgriffith$elm_ui$Internal$Style$Prop, 'pointer-events', 'auto')
												]))
										]));
							case 3:
								return A2(
									$mdgriffith$elm_ui$Internal$Style$Descriptor,
									$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.cF),
									_List_fromArray(
										[
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'position', 'absolute'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'right', '100%'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'top', '0'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'height', '100%'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin', '0 !important'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'z-index', '20'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'pointer-events', 'none'),
											A2(
											$mdgriffith$elm_ui$Internal$Style$Child,
											'*',
											_List_fromArray(
												[
													A2($mdgriffith$elm_ui$Internal$Style$Prop, 'pointer-events', 'auto')
												]))
										]));
							case 4:
								return A2(
									$mdgriffith$elm_ui$Internal$Style$Descriptor,
									$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.ct),
									_List_fromArray(
										[
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'position', 'absolute'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'width', '100%'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'height', '100%'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'left', '0'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'top', '0'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin', '0 !important'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'pointer-events', 'none'),
											A2(
											$mdgriffith$elm_ui$Internal$Style$Child,
											'*',
											_List_fromArray(
												[
													A2($mdgriffith$elm_ui$Internal$Style$Prop, 'pointer-events', 'auto')
												]))
										]));
							default:
								return A2(
									$mdgriffith$elm_ui$Internal$Style$Descriptor,
									$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.bW),
									_List_fromArray(
										[
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'position', 'absolute'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'width', '100%'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'height', '100%'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'left', '0'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'top', '0'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin', '0 !important'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'z-index', '0'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'pointer-events', 'none'),
											A2(
											$mdgriffith$elm_ui$Internal$Style$Child,
											'*',
											_List_fromArray(
												[
													A2($mdgriffith$elm_ui$Internal$Style$Prop, 'pointer-events', 'auto')
												]))
										]));
						}
					}))
			])),
		A2(
		$mdgriffith$elm_ui$Internal$Style$Class,
		$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.bS),
		_List_fromArray(
			[
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'position', 'relative'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'border', 'none'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-shrink', '0'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'flex'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-direction', 'row'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-basis', 'auto'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'resize', 'none'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'font-feature-settings', 'inherit'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'box-sizing', 'border-box'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin', '0'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'padding', '0'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'border-width', '0'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'border-style', 'solid'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'font-size', 'inherit'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'color', 'inherit'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'font-family', 'inherit'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'line-height', '1'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'font-weight', 'inherit'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'text-decoration', 'none'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'font-style', 'inherit'),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.aU),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-wrap', 'wrap')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.bf),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, '-moz-user-select', 'none'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, '-webkit-user-select', 'none'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, '-ms-user-select', 'none'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'user-select', 'none')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.cd),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'cursor', 'pointer')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.ce),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'cursor', 'text')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.cN),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'pointer-events', 'none !important')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.an),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'pointer-events', 'auto !important')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.P),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'opacity', '0')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.M),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'opacity', '1')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot(
					_Utils_ap($mdgriffith$elm_ui$Internal$Style$classes.cp, $mdgriffith$elm_ui$Internal$Style$classes.P)) + ':hover',
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'opacity', '0')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot(
					_Utils_ap($mdgriffith$elm_ui$Internal$Style$classes.cp, $mdgriffith$elm_ui$Internal$Style$classes.M)) + ':hover',
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'opacity', '1')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot(
					_Utils_ap($mdgriffith$elm_ui$Internal$Style$classes.cl, $mdgriffith$elm_ui$Internal$Style$classes.P)) + ':focus',
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'opacity', '0')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot(
					_Utils_ap($mdgriffith$elm_ui$Internal$Style$classes.cl, $mdgriffith$elm_ui$Internal$Style$classes.M)) + ':focus',
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'opacity', '1')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot(
					_Utils_ap($mdgriffith$elm_ui$Internal$Style$classes.az, $mdgriffith$elm_ui$Internal$Style$classes.P)) + ':active',
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'opacity', '0')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot(
					_Utils_ap($mdgriffith$elm_ui$Internal$Style$classes.az, $mdgriffith$elm_ui$Internal$Style$classes.M)) + ':active',
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'opacity', '1')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.by),
				_List_fromArray(
					[
						A2(
						$mdgriffith$elm_ui$Internal$Style$Prop,
						'transition',
						A2(
							$elm$core$String$join,
							', ',
							A2(
								$elm$core$List$map,
								function (x) {
									return x + ' 160ms';
								},
								_List_fromArray(
									['transform', 'opacity', 'filter', 'background-color', 'color', 'font-size']))))
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.cS),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'overflow', 'auto'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-shrink', '1')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.cT),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'overflow-x', 'auto'),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Descriptor,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.s),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-shrink', '1')
							]))
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.cU),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'overflow-y', 'auto'),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Descriptor,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.B),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-shrink', '1')
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Descriptor,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.cX),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-shrink', '1')
							]))
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.b8),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'overflow', 'hidden')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.b9),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'overflow-x', 'hidden')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.ca),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'overflow-y', 'hidden')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.aS),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'width', 'auto')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.am),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'border-width', '0')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.b$),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'border-style', 'dashed')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.b0),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'border-style', 'dotted')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.b1),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'border-style', 'solid')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.c6),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'white-space', 'pre'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'inline-block')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.cz),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'line-height', '1.05'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'background', 'transparent')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.cX),
				$mdgriffith$elm_ui$Internal$Style$elDescription),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.s),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'flex'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-direction', 'row'),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.bS),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-basis', '0%'),
								A2(
								$mdgriffith$elm_ui$Internal$Style$Descriptor,
								$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.bC),
								_List_fromArray(
									[
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-basis', 'auto')
									])),
								A2(
								$mdgriffith$elm_ui$Internal$Style$Descriptor,
								$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.bc),
								_List_fromArray(
									[
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-basis', 'auto')
									]))
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.aI),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'stretch !important')
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.a5),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'stretch !important')
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.aT),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '100000')
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.ap),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '0'),
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-basis', 'auto'),
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'stretch')
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						'u:first-of-type.' + $mdgriffith$elm_ui$Internal$Style$classes.bP,
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '1')
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						's:first-of-type.' + $mdgriffith$elm_ui$Internal$Style$classes.bN,
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '1'),
								A2(
								$mdgriffith$elm_ui$Internal$Style$Child,
								$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.bK),
								_List_fromArray(
									[
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin-left', 'auto !important')
									]))
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						's:last-of-type.' + $mdgriffith$elm_ui$Internal$Style$classes.bN,
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '1'),
								A2(
								$mdgriffith$elm_ui$Internal$Style$Child,
								$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.bK),
								_List_fromArray(
									[
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin-right', 'auto !important')
									]))
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						's:only-of-type.' + $mdgriffith$elm_ui$Internal$Style$classes.bN,
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '1'),
								A2(
								$mdgriffith$elm_ui$Internal$Style$Child,
								$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.bL),
								_List_fromArray(
									[
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin-top', 'auto !important'),
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin-bottom', 'auto !important')
									]))
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						's:last-of-type.' + ($mdgriffith$elm_ui$Internal$Style$classes.bN + ' ~ u'),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '0')
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						'u:first-of-type.' + ($mdgriffith$elm_ui$Internal$Style$classes.bP + (' ~ s.' + $mdgriffith$elm_ui$Internal$Style$classes.bN)),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '0')
							])),
						$mdgriffith$elm_ui$Internal$Style$describeAlignment(
						function (alignment) {
							switch (alignment) {
								case 0:
									return _Utils_Tuple2(
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-items', 'flex-start')
											]),
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'flex-start')
											]));
								case 1:
									return _Utils_Tuple2(
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-items', 'flex-end')
											]),
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'flex-end')
											]));
								case 2:
									return _Utils_Tuple2(
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'justify-content', 'flex-end')
											]),
										_List_Nil);
								case 3:
									return _Utils_Tuple2(
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'justify-content', 'flex-start')
											]),
										_List_Nil);
								case 4:
									return _Utils_Tuple2(
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'justify-content', 'center')
											]),
										_List_Nil);
								default:
									return _Utils_Tuple2(
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-items', 'center')
											]),
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'center')
											]));
							}
						}),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Descriptor,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.cZ),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'justify-content', 'space-between')
							]))
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.B),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'flex'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-direction', 'column'),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.bS),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-basis', '0%'),
								A2(
								$mdgriffith$elm_ui$Internal$Style$Descriptor,
								$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.a4),
								_List_fromArray(
									[
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-basis', 'auto')
									])),
								A2(
								$mdgriffith$elm_ui$Internal$Style$Descriptor,
								$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.B),
								_List_fromArray(
									[
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-basis', 'auto')
									]))
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.aI),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '100000')
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.aT),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'width', '100%')
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.bD),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'width', '100%')
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.aS),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'flex-start')
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						'u:first-of-type.' + $mdgriffith$elm_ui$Internal$Style$classes.bM,
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '1')
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						's:first-of-type.' + $mdgriffith$elm_ui$Internal$Style$classes.bO,
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '1'),
								A2(
								$mdgriffith$elm_ui$Internal$Style$Child,
								$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.bL),
								_List_fromArray(
									[
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin-top', 'auto !important'),
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin-bottom', '0 !important')
									]))
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						's:last-of-type.' + $mdgriffith$elm_ui$Internal$Style$classes.bO,
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '1'),
								A2(
								$mdgriffith$elm_ui$Internal$Style$Child,
								$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.bL),
								_List_fromArray(
									[
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin-bottom', 'auto !important'),
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin-top', '0 !important')
									]))
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						's:only-of-type.' + $mdgriffith$elm_ui$Internal$Style$classes.bO,
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '1'),
								A2(
								$mdgriffith$elm_ui$Internal$Style$Child,
								$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.bL),
								_List_fromArray(
									[
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin-top', 'auto !important'),
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin-bottom', 'auto !important')
									]))
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						's:last-of-type.' + ($mdgriffith$elm_ui$Internal$Style$classes.bO + ' ~ u'),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '0')
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						'u:first-of-type.' + ($mdgriffith$elm_ui$Internal$Style$classes.bM + (' ~ s.' + $mdgriffith$elm_ui$Internal$Style$classes.bO)),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '0')
							])),
						$mdgriffith$elm_ui$Internal$Style$describeAlignment(
						function (alignment) {
							switch (alignment) {
								case 0:
									return _Utils_Tuple2(
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'justify-content', 'flex-start')
											]),
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin-bottom', 'auto')
											]));
								case 1:
									return _Utils_Tuple2(
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'justify-content', 'flex-end')
											]),
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin-top', 'auto')
											]));
								case 2:
									return _Utils_Tuple2(
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-items', 'flex-end')
											]),
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'flex-end')
											]));
								case 3:
									return _Utils_Tuple2(
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-items', 'flex-start')
											]),
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'flex-start')
											]));
								case 4:
									return _Utils_Tuple2(
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-items', 'center')
											]),
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'center')
											]));
								default:
									return _Utils_Tuple2(
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'justify-content', 'center')
											]),
										_List_Nil);
							}
						}),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.ap),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '0'),
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-basis', 'auto'),
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'width', '100%'),
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'stretch !important')
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Descriptor,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.cZ),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'justify-content', 'space-between')
							]))
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.cn),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', '-ms-grid'),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						'.gp',
						_List_fromArray(
							[
								A2(
								$mdgriffith$elm_ui$Internal$Style$Child,
								$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.bS),
								_List_fromArray(
									[
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'width', '100%')
									]))
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Supports,
						_Utils_Tuple2('display', 'grid'),
						_List_fromArray(
							[
								_Utils_Tuple2('display', 'grid')
							])),
						$mdgriffith$elm_ui$Internal$Style$gridAlignments(
						function (alignment) {
							switch (alignment) {
								case 0:
									return _List_fromArray(
										[
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'justify-content', 'flex-start')
										]);
								case 1:
									return _List_fromArray(
										[
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'justify-content', 'flex-end')
										]);
								case 2:
									return _List_fromArray(
										[
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-items', 'flex-end')
										]);
								case 3:
									return _List_fromArray(
										[
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-items', 'flex-start')
										]);
								case 4:
									return _List_fromArray(
										[
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-items', 'center')
										]);
								default:
									return _List_fromArray(
										[
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'justify-content', 'center')
										]);
							}
						})
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.bi),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'block'),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.bS + ':first-child'),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin', '0 !important')
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						$mdgriffith$elm_ui$Internal$Style$dot(
							$mdgriffith$elm_ui$Internal$Style$classes.bS + ($mdgriffith$elm_ui$Internal$Style$selfName(3) + (':first-child + .' + $mdgriffith$elm_ui$Internal$Style$classes.bS))),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin', '0 !important')
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						$mdgriffith$elm_ui$Internal$Style$dot(
							$mdgriffith$elm_ui$Internal$Style$classes.bS + ($mdgriffith$elm_ui$Internal$Style$selfName(2) + (':first-child + .' + $mdgriffith$elm_ui$Internal$Style$classes.bS))),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin', '0 !important')
							])),
						$mdgriffith$elm_ui$Internal$Style$describeAlignment(
						function (alignment) {
							switch (alignment) {
								case 0:
									return _Utils_Tuple2(_List_Nil, _List_Nil);
								case 1:
									return _Utils_Tuple2(_List_Nil, _List_Nil);
								case 2:
									return _Utils_Tuple2(
										_List_Nil,
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'float', 'right'),
												A2(
												$mdgriffith$elm_ui$Internal$Style$Descriptor,
												'::after',
												_List_fromArray(
													[
														A2($mdgriffith$elm_ui$Internal$Style$Prop, 'content', '\"\"'),
														A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'table'),
														A2($mdgriffith$elm_ui$Internal$Style$Prop, 'clear', 'both')
													]))
											]));
								case 3:
									return _Utils_Tuple2(
										_List_Nil,
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'float', 'left'),
												A2(
												$mdgriffith$elm_ui$Internal$Style$Descriptor,
												'::after',
												_List_fromArray(
													[
														A2($mdgriffith$elm_ui$Internal$Style$Prop, 'content', '\"\"'),
														A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'table'),
														A2($mdgriffith$elm_ui$Internal$Style$Prop, 'clear', 'both')
													]))
											]));
								case 4:
									return _Utils_Tuple2(_List_Nil, _List_Nil);
								default:
									return _Utils_Tuple2(_List_Nil, _List_Nil);
							}
						})
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.cv),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'white-space', 'pre-wrap'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'height', '100%'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'width', '100%'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'background-color', 'transparent')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.cy),
				_List_fromArray(
					[
						A2(
						$mdgriffith$elm_ui$Internal$Style$Descriptor,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.cX),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-basis', 'auto')
							]))
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.cx),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'white-space', 'pre-wrap'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'cursor', 'text'),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.cw),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'white-space', 'pre-wrap'),
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'color', 'transparent')
							]))
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.bj),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'block'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'white-space', 'normal'),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Descriptor,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.aF),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'z-index', '0'),
								A2(
								$mdgriffith$elm_ui$Internal$Style$Child,
								$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.bW),
								_List_fromArray(
									[
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'z-index', '-1')
									]))
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.c6),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'inline'),
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'white-space', 'normal')
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.cX),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'inline'),
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'white-space', 'normal'),
								A2(
								$mdgriffith$elm_ui$Internal$Style$Descriptor,
								$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.ct),
								_List_fromArray(
									[
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'flex')
									])),
								A2(
								$mdgriffith$elm_ui$Internal$Style$Descriptor,
								$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.bW),
								_List_fromArray(
									[
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'flex')
									])),
								A2(
								$mdgriffith$elm_ui$Internal$Style$Descriptor,
								$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.bH),
								_List_fromArray(
									[
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'flex')
									])),
								A2(
								$mdgriffith$elm_ui$Internal$Style$Descriptor,
								$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.bX),
								_List_fromArray(
									[
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'flex')
									])),
								A2(
								$mdgriffith$elm_ui$Internal$Style$Descriptor,
								$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.cH),
								_List_fromArray(
									[
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'flex')
									])),
								A2(
								$mdgriffith$elm_ui$Internal$Style$Descriptor,
								$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.cF),
								_List_fromArray(
									[
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'flex')
									])),
								A2(
								$mdgriffith$elm_ui$Internal$Style$Child,
								$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.c6),
								_List_fromArray(
									[
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'inline'),
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'white-space', 'normal')
									])),
								A2(
								$mdgriffith$elm_ui$Internal$Style$Child,
								$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.cX),
								_List_fromArray(
									[
										A2(
										$mdgriffith$elm_ui$Internal$Style$Child,
										$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.c6),
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'inline'),
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'white-space', 'normal')
											]))
									]))
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.s),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'inline-flex')
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.B),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'inline-flex')
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.cn),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'inline-grid')
							])),
						$mdgriffith$elm_ui$Internal$Style$describeAlignment(
						function (alignment) {
							switch (alignment) {
								case 0:
									return _Utils_Tuple2(_List_Nil, _List_Nil);
								case 1:
									return _Utils_Tuple2(_List_Nil, _List_Nil);
								case 2:
									return _Utils_Tuple2(
										_List_Nil,
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'float', 'right')
											]));
								case 3:
									return _Utils_Tuple2(
										_List_Nil,
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'float', 'left')
											]));
								case 4:
									return _Utils_Tuple2(_List_Nil, _List_Nil);
								default:
									return _Utils_Tuple2(_List_Nil, _List_Nil);
							}
						})
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				'.hidden',
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'none')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.di),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'font-weight', '100')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.c9),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'font-weight', '200')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.dd),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'font-weight', '300')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.df),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'font-weight', '400')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.de),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'font-weight', '500')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.dh),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'font-weight', '600')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.bZ),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'font-weight', '700')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.c8),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'font-weight', '800')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.da),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'font-weight', '900')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.cA),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'font-style', 'italic')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.c3),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'text-decoration', 'line-through')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.dp),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'text-decoration', 'underline'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'text-decoration-skip-ink', 'auto'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'text-decoration-skip', 'ink')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				_Utils_ap(
					$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.dp),
					$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.c3)),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'text-decoration', 'line-through underline'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'text-decoration-skip-ink', 'auto'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'text-decoration-skip', 'ink')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.dj),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'font-style', 'normal')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.db),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'text-align', 'justify')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.aw),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'text-align', 'justify-all')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.c7),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'text-align', 'center')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.dg),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'text-align', 'right')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.dc),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'text-align', 'left')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				'.modal',
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'position', 'fixed'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'left', '0'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'top', '0'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'width', '100%'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'height', '100%'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'pointer-events', 'none')
					]))
			]))
	]);
var $mdgriffith$elm_ui$Internal$Style$fontVariant = function (_var) {
	return _List_fromArray(
		[
			A2(
			$mdgriffith$elm_ui$Internal$Style$Class,
			'.v-' + _var,
			_List_fromArray(
				[
					A2($mdgriffith$elm_ui$Internal$Style$Prop, 'font-feature-settings', '\"' + (_var + '\"'))
				])),
			A2(
			$mdgriffith$elm_ui$Internal$Style$Class,
			'.v-' + (_var + '-off'),
			_List_fromArray(
				[
					A2($mdgriffith$elm_ui$Internal$Style$Prop, 'font-feature-settings', '\"' + (_var + '\" 0'))
				]))
		]);
};
var $mdgriffith$elm_ui$Internal$Style$commonValues = $elm$core$List$concat(
	_List_fromArray(
		[
			A2(
			$elm$core$List$map,
			function (x) {
				return A2(
					$mdgriffith$elm_ui$Internal$Style$Class,
					'.border-' + $elm$core$String$fromInt(x),
					_List_fromArray(
						[
							A2(
							$mdgriffith$elm_ui$Internal$Style$Prop,
							'border-width',
							$elm$core$String$fromInt(x) + 'px')
						]));
			},
			A2($elm$core$List$range, 0, 6)),
			A2(
			$elm$core$List$map,
			function (i) {
				return A2(
					$mdgriffith$elm_ui$Internal$Style$Class,
					'.font-size-' + $elm$core$String$fromInt(i),
					_List_fromArray(
						[
							A2(
							$mdgriffith$elm_ui$Internal$Style$Prop,
							'font-size',
							$elm$core$String$fromInt(i) + 'px')
						]));
			},
			A2($elm$core$List$range, 8, 32)),
			A2(
			$elm$core$List$map,
			function (i) {
				return A2(
					$mdgriffith$elm_ui$Internal$Style$Class,
					'.p-' + $elm$core$String$fromInt(i),
					_List_fromArray(
						[
							A2(
							$mdgriffith$elm_ui$Internal$Style$Prop,
							'padding',
							$elm$core$String$fromInt(i) + 'px')
						]));
			},
			A2($elm$core$List$range, 0, 24)),
			_List_fromArray(
			[
				A2(
				$mdgriffith$elm_ui$Internal$Style$Class,
				'.v-smcp',
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'font-variant', 'small-caps')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Class,
				'.v-smcp-off',
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'font-variant', 'normal')
					]))
			]),
			$mdgriffith$elm_ui$Internal$Style$fontVariant('zero'),
			$mdgriffith$elm_ui$Internal$Style$fontVariant('onum'),
			$mdgriffith$elm_ui$Internal$Style$fontVariant('liga'),
			$mdgriffith$elm_ui$Internal$Style$fontVariant('dlig'),
			$mdgriffith$elm_ui$Internal$Style$fontVariant('ordn'),
			$mdgriffith$elm_ui$Internal$Style$fontVariant('tnum'),
			$mdgriffith$elm_ui$Internal$Style$fontVariant('afrc'),
			$mdgriffith$elm_ui$Internal$Style$fontVariant('frac')
		]));
var $mdgriffith$elm_ui$Internal$Style$explainer = '\n.explain {\n    border: 6px solid rgb(174, 121, 15) !important;\n}\n.explain > .' + ($mdgriffith$elm_ui$Internal$Style$classes.bS + (' {\n    border: 4px dashed rgb(0, 151, 167) !important;\n}\n\n.ctr {\n    border: none !important;\n}\n.explain > .ctr > .' + ($mdgriffith$elm_ui$Internal$Style$classes.bS + ' {\n    border: 4px dashed rgb(0, 151, 167) !important;\n}\n\n')));
var $mdgriffith$elm_ui$Internal$Style$inputTextReset = '\ninput[type="search"],\ninput[type="search"]::-webkit-search-decoration,\ninput[type="search"]::-webkit-search-cancel-button,\ninput[type="search"]::-webkit-search-results-button,\ninput[type="search"]::-webkit-search-results-decoration {\n  -webkit-appearance:none;\n}\n';
var $mdgriffith$elm_ui$Internal$Style$sliderReset = '\ninput[type=range] {\n  -webkit-appearance: none; \n  background: transparent;\n  position:absolute;\n  left:0;\n  top:0;\n  z-index:10;\n  width: 100%;\n  outline: dashed 1px;\n  height: 100%;\n  opacity: 0;\n}\n';
var $mdgriffith$elm_ui$Internal$Style$thumbReset = '\ninput[type=range]::-webkit-slider-thumb {\n    -webkit-appearance: none;\n    opacity: 0.5;\n    width: 80px;\n    height: 80px;\n    background-color: black;\n    border:none;\n    border-radius: 5px;\n}\ninput[type=range]::-moz-range-thumb {\n    opacity: 0.5;\n    width: 80px;\n    height: 80px;\n    background-color: black;\n    border:none;\n    border-radius: 5px;\n}\ninput[type=range]::-ms-thumb {\n    opacity: 0.5;\n    width: 80px;\n    height: 80px;\n    background-color: black;\n    border:none;\n    border-radius: 5px;\n}\ninput[type=range][orient=vertical]{\n    writing-mode: bt-lr; /* IE */\n    -webkit-appearance: slider-vertical;  /* WebKit */\n}\n';
var $mdgriffith$elm_ui$Internal$Style$trackReset = '\ninput[type=range]::-moz-range-track {\n    background: transparent;\n    cursor: pointer;\n}\ninput[type=range]::-ms-track {\n    background: transparent;\n    cursor: pointer;\n}\ninput[type=range]::-webkit-slider-runnable-track {\n    background: transparent;\n    cursor: pointer;\n}\n';
var $mdgriffith$elm_ui$Internal$Style$overrides = '@media screen and (-ms-high-contrast: active), (-ms-high-contrast: none) {' + ($mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.bS) + ($mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.s) + (' > ' + ($mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.bS) + (' { flex-basis: auto !important; } ' + ($mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.bS) + ($mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.s) + (' > ' + ($mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.bS) + ($mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.ap) + (' { flex-basis: auto !important; }}' + ($mdgriffith$elm_ui$Internal$Style$inputTextReset + ($mdgriffith$elm_ui$Internal$Style$sliderReset + ($mdgriffith$elm_ui$Internal$Style$trackReset + ($mdgriffith$elm_ui$Internal$Style$thumbReset + $mdgriffith$elm_ui$Internal$Style$explainer)))))))))))))));
var $elm$core$String$concat = function (strings) {
	return A2($elm$core$String$join, '', strings);
};
var $mdgriffith$elm_ui$Internal$Style$Intermediate = $elm$core$Basics$identity;
var $mdgriffith$elm_ui$Internal$Style$emptyIntermediate = F2(
	function (selector, closing) {
		return {ao: closing, i: _List_Nil, E: _List_Nil, y: selector};
	});
var $mdgriffith$elm_ui$Internal$Style$renderRules = F2(
	function (_v0, rulesToRender) {
		var parent = _v0;
		var generateIntermediates = F2(
			function (rule, rendered) {
				switch (rule.$) {
					case 0:
						var name = rule.a;
						var val = rule.b;
						return _Utils_update(
							rendered,
							{
								E: A2(
									$elm$core$List$cons,
									_Utils_Tuple2(name, val),
									rendered.E)
							});
					case 2:
						var _v2 = rule.a;
						var prop = _v2.a;
						var value = _v2.b;
						var props = rule.b;
						return _Utils_update(
							rendered,
							{
								i: A2(
									$elm$core$List$cons,
									{ao: '\n}', i: _List_Nil, E: props, y: '@supports (' + (prop + (':' + (value + (') {' + parent.y))))},
									rendered.i)
							});
					case 4:
						var selector = rule.a;
						var adjRules = rule.b;
						return _Utils_update(
							rendered,
							{
								i: A2(
									$elm$core$List$cons,
									A2(
										$mdgriffith$elm_ui$Internal$Style$renderRules,
										A2($mdgriffith$elm_ui$Internal$Style$emptyIntermediate, parent.y + (' + ' + selector), ''),
										adjRules),
									rendered.i)
							});
					case 1:
						var child = rule.a;
						var childRules = rule.b;
						return _Utils_update(
							rendered,
							{
								i: A2(
									$elm$core$List$cons,
									A2(
										$mdgriffith$elm_ui$Internal$Style$renderRules,
										A2($mdgriffith$elm_ui$Internal$Style$emptyIntermediate, parent.y + (' > ' + child), ''),
										childRules),
									rendered.i)
							});
					case 3:
						var descriptor = rule.a;
						var descriptorRules = rule.b;
						return _Utils_update(
							rendered,
							{
								i: A2(
									$elm$core$List$cons,
									A2(
										$mdgriffith$elm_ui$Internal$Style$renderRules,
										A2(
											$mdgriffith$elm_ui$Internal$Style$emptyIntermediate,
											_Utils_ap(parent.y, descriptor),
											''),
										descriptorRules),
									rendered.i)
							});
					default:
						var batched = rule.a;
						return _Utils_update(
							rendered,
							{
								i: A2(
									$elm$core$List$cons,
									A2(
										$mdgriffith$elm_ui$Internal$Style$renderRules,
										A2($mdgriffith$elm_ui$Internal$Style$emptyIntermediate, parent.y, ''),
										batched),
									rendered.i)
							});
				}
			});
		return A3($elm$core$List$foldr, generateIntermediates, parent, rulesToRender);
	});
var $mdgriffith$elm_ui$Internal$Style$renderCompact = function (styleClasses) {
	var renderValues = function (values) {
		return $elm$core$String$concat(
			A2(
				$elm$core$List$map,
				function (_v3) {
					var x = _v3.a;
					var y = _v3.b;
					return x + (':' + (y + ';'));
				},
				values));
	};
	var renderClass = function (rule) {
		var _v2 = rule.E;
		if (!_v2.b) {
			return '';
		} else {
			return rule.y + ('{' + (renderValues(rule.E) + (rule.ao + '}')));
		}
	};
	var renderIntermediate = function (_v0) {
		var rule = _v0;
		return _Utils_ap(
			renderClass(rule),
			$elm$core$String$concat(
				A2($elm$core$List$map, renderIntermediate, rule.i)));
	};
	return $elm$core$String$concat(
		A2(
			$elm$core$List$map,
			renderIntermediate,
			A3(
				$elm$core$List$foldr,
				F2(
					function (_v1, existing) {
						var name = _v1.a;
						var styleRules = _v1.b;
						return A2(
							$elm$core$List$cons,
							A2(
								$mdgriffith$elm_ui$Internal$Style$renderRules,
								A2($mdgriffith$elm_ui$Internal$Style$emptyIntermediate, name, ''),
								styleRules),
							existing);
					}),
				_List_Nil,
				styleClasses)));
};
var $mdgriffith$elm_ui$Internal$Style$rules = _Utils_ap(
	$mdgriffith$elm_ui$Internal$Style$overrides,
	$mdgriffith$elm_ui$Internal$Style$renderCompact(
		_Utils_ap($mdgriffith$elm_ui$Internal$Style$baseSheet, $mdgriffith$elm_ui$Internal$Style$commonValues)));
var $elm$virtual_dom$VirtualDom$text = _VirtualDom_text;
var $mdgriffith$elm_ui$Internal$Model$staticRoot = function (opts) {
	var _v0 = opts.cC;
	switch (_v0) {
		case 0:
			return A3(
				$elm$virtual_dom$VirtualDom$node,
				'div',
				_List_Nil,
				_List_fromArray(
					[
						A3(
						$elm$virtual_dom$VirtualDom$node,
						'style',
						_List_Nil,
						_List_fromArray(
							[
								$elm$virtual_dom$VirtualDom$text($mdgriffith$elm_ui$Internal$Style$rules)
							]))
					]));
		case 1:
			return $elm$virtual_dom$VirtualDom$text('');
		default:
			return A3(
				$elm$virtual_dom$VirtualDom$node,
				'elm-ui-static-rules',
				_List_fromArray(
					[
						A2(
						$elm$virtual_dom$VirtualDom$property,
						'rules',
						$elm$json$Json$Encode$string($mdgriffith$elm_ui$Internal$Style$rules))
					]),
				_List_Nil);
	}
};
var $elm$json$Json$Encode$list = F2(
	function (func, entries) {
		return _Json_wrap(
			A3(
				$elm$core$List$foldl,
				_Json_addEntry(func),
				_Json_emptyArray(0),
				entries));
	});
var $elm$json$Json$Encode$object = function (pairs) {
	return _Json_wrap(
		A3(
			$elm$core$List$foldl,
			F2(
				function (_v0, obj) {
					var k = _v0.a;
					var v = _v0.b;
					return A3(_Json_addField, k, v, obj);
				}),
			_Json_emptyObject(0),
			pairs));
};
var $elm$core$List$any = F2(
	function (isOkay, list) {
		any:
		while (true) {
			if (!list.b) {
				return false;
			} else {
				var x = list.a;
				var xs = list.b;
				if (isOkay(x)) {
					return true;
				} else {
					var $temp$isOkay = isOkay,
						$temp$list = xs;
					isOkay = $temp$isOkay;
					list = $temp$list;
					continue any;
				}
			}
		}
	});
var $mdgriffith$elm_ui$Internal$Model$fontName = function (font) {
	switch (font.$) {
		case 0:
			return 'serif';
		case 1:
			return 'sans-serif';
		case 2:
			return 'monospace';
		case 3:
			var name = font.a;
			return '\"' + (name + '\"');
		case 4:
			var name = font.a;
			var url = font.b;
			return '\"' + (name + '\"');
		default:
			var name = font.a.cD;
			return '\"' + (name + '\"');
	}
};
var $mdgriffith$elm_ui$Internal$Model$isSmallCaps = function (_var) {
	switch (_var.$) {
		case 0:
			var name = _var.a;
			return name === 'smcp';
		case 1:
			var name = _var.a;
			return false;
		default:
			var name = _var.a;
			var index = _var.b;
			return (name === 'smcp') && (index === 1);
	}
};
var $mdgriffith$elm_ui$Internal$Model$hasSmallCaps = function (typeface) {
	if (typeface.$ === 5) {
		var font = typeface.a;
		return A2($elm$core$List$any, $mdgriffith$elm_ui$Internal$Model$isSmallCaps, font.bz);
	} else {
		return false;
	}
};
var $elm$core$Basics$min = F2(
	function (x, y) {
		return (_Utils_cmp(x, y) < 0) ? x : y;
	});
var $mdgriffith$elm_ui$Internal$Model$renderProps = F3(
	function (force, _v0, existing) {
		var key = _v0.a;
		var val = _v0.b;
		return force ? (existing + ('\n  ' + (key + (': ' + (val + ' !important;'))))) : (existing + ('\n  ' + (key + (': ' + (val + ';')))));
	});
var $mdgriffith$elm_ui$Internal$Model$renderStyle = F4(
	function (options, maybePseudo, selector, props) {
		if (maybePseudo.$ === 1) {
			return _List_fromArray(
				[
					selector + ('{' + (A3(
					$elm$core$List$foldl,
					$mdgriffith$elm_ui$Internal$Model$renderProps(false),
					'',
					props) + '\n}'))
				]);
		} else {
			var pseudo = maybePseudo.a;
			switch (pseudo) {
				case 1:
					var _v2 = options.cp;
					switch (_v2) {
						case 0:
							return _List_Nil;
						case 2:
							return _List_fromArray(
								[
									selector + ('-hv {' + (A3(
									$elm$core$List$foldl,
									$mdgriffith$elm_ui$Internal$Model$renderProps(true),
									'',
									props) + '\n}'))
								]);
						default:
							return _List_fromArray(
								[
									selector + ('-hv:hover {' + (A3(
									$elm$core$List$foldl,
									$mdgriffith$elm_ui$Internal$Model$renderProps(false),
									'',
									props) + '\n}'))
								]);
					}
				case 0:
					var renderedProps = A3(
						$elm$core$List$foldl,
						$mdgriffith$elm_ui$Internal$Model$renderProps(false),
						'',
						props);
					return _List_fromArray(
						[selector + ('-fs:focus {' + (renderedProps + '\n}')), '.' + ($mdgriffith$elm_ui$Internal$Style$classes.bS + (':focus ~ ' + (selector + ('-fs:not(.focus)  {' + (renderedProps + '\n}'))))), '.' + ($mdgriffith$elm_ui$Internal$Style$classes.bS + (':focus ' + (selector + ('-fs  {' + (renderedProps + '\n}'))))), selector + ('-fs:focus-within {' + (renderedProps + '\n}')), '.focusable-parent:focus ~ ' + ('.' + ($mdgriffith$elm_ui$Internal$Style$classes.bS + (' ' + (selector + ('-fs {' + (renderedProps + '\n}'))))))]);
				default:
					return _List_fromArray(
						[
							selector + ('-act:active {' + (A3(
							$elm$core$List$foldl,
							$mdgriffith$elm_ui$Internal$Model$renderProps(false),
							'',
							props) + '\n}'))
						]);
			}
		}
	});
var $mdgriffith$elm_ui$Internal$Model$renderVariant = function (_var) {
	switch (_var.$) {
		case 0:
			var name = _var.a;
			return '\"' + (name + '\"');
		case 1:
			var name = _var.a;
			return '\"' + (name + '\" 0');
		default:
			var name = _var.a;
			var index = _var.b;
			return '\"' + (name + ('\" ' + $elm$core$String$fromInt(index)));
	}
};
var $mdgriffith$elm_ui$Internal$Model$renderVariants = function (typeface) {
	if (typeface.$ === 5) {
		var font = typeface.a;
		return $elm$core$Maybe$Just(
			A2(
				$elm$core$String$join,
				', ',
				A2($elm$core$List$map, $mdgriffith$elm_ui$Internal$Model$renderVariant, font.bz)));
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $mdgriffith$elm_ui$Internal$Model$transformValue = function (transform) {
	switch (transform.$) {
		case 0:
			return $elm$core$Maybe$Nothing;
		case 1:
			var _v1 = transform.a;
			var x = _v1.a;
			var y = _v1.b;
			var z = _v1.c;
			return $elm$core$Maybe$Just(
				'translate3d(' + ($elm$core$String$fromFloat(x) + ('px, ' + ($elm$core$String$fromFloat(y) + ('px, ' + ($elm$core$String$fromFloat(z) + 'px)'))))));
		default:
			var _v2 = transform.a;
			var tx = _v2.a;
			var ty = _v2.b;
			var tz = _v2.c;
			var _v3 = transform.b;
			var sx = _v3.a;
			var sy = _v3.b;
			var sz = _v3.c;
			var _v4 = transform.c;
			var ox = _v4.a;
			var oy = _v4.b;
			var oz = _v4.c;
			var angle = transform.d;
			var translate = 'translate3d(' + ($elm$core$String$fromFloat(tx) + ('px, ' + ($elm$core$String$fromFloat(ty) + ('px, ' + ($elm$core$String$fromFloat(tz) + 'px)')))));
			var scale = 'scale3d(' + ($elm$core$String$fromFloat(sx) + (', ' + ($elm$core$String$fromFloat(sy) + (', ' + ($elm$core$String$fromFloat(sz) + ')')))));
			var rotate = 'rotate3d(' + ($elm$core$String$fromFloat(ox) + (', ' + ($elm$core$String$fromFloat(oy) + (', ' + ($elm$core$String$fromFloat(oz) + (', ' + ($elm$core$String$fromFloat(angle) + 'rad)')))))));
			return $elm$core$Maybe$Just(translate + (' ' + (scale + (' ' + rotate))));
	}
};
var $mdgriffith$elm_ui$Internal$Model$renderStyleRule = F3(
	function (options, rule, maybePseudo) {
		switch (rule.$) {
			case 0:
				var selector = rule.a;
				var props = rule.b;
				return A4($mdgriffith$elm_ui$Internal$Model$renderStyle, options, maybePseudo, selector, props);
			case 13:
				var name = rule.a;
				var prop = rule.b;
				return A4(
					$mdgriffith$elm_ui$Internal$Model$renderStyle,
					options,
					maybePseudo,
					'.' + name,
					_List_fromArray(
						[
							A2($mdgriffith$elm_ui$Internal$Model$Property, 'box-shadow', prop)
						]));
			case 12:
				var name = rule.a;
				var transparency = rule.b;
				var opacity = A2(
					$elm$core$Basics$max,
					0,
					A2($elm$core$Basics$min, 1, 1 - transparency));
				return A4(
					$mdgriffith$elm_ui$Internal$Model$renderStyle,
					options,
					maybePseudo,
					'.' + name,
					_List_fromArray(
						[
							A2(
							$mdgriffith$elm_ui$Internal$Model$Property,
							'opacity',
							$elm$core$String$fromFloat(opacity))
						]));
			case 2:
				var i = rule.a;
				return A4(
					$mdgriffith$elm_ui$Internal$Model$renderStyle,
					options,
					maybePseudo,
					'.font-size-' + $elm$core$String$fromInt(i),
					_List_fromArray(
						[
							A2(
							$mdgriffith$elm_ui$Internal$Model$Property,
							'font-size',
							$elm$core$String$fromInt(i) + 'px')
						]));
			case 1:
				var name = rule.a;
				var typefaces = rule.b;
				var features = A2(
					$elm$core$String$join,
					', ',
					A2($elm$core$List$filterMap, $mdgriffith$elm_ui$Internal$Model$renderVariants, typefaces));
				var families = _List_fromArray(
					[
						A2(
						$mdgriffith$elm_ui$Internal$Model$Property,
						'font-family',
						A2(
							$elm$core$String$join,
							', ',
							A2($elm$core$List$map, $mdgriffith$elm_ui$Internal$Model$fontName, typefaces))),
						A2($mdgriffith$elm_ui$Internal$Model$Property, 'font-feature-settings', features),
						A2(
						$mdgriffith$elm_ui$Internal$Model$Property,
						'font-variant',
						A2($elm$core$List$any, $mdgriffith$elm_ui$Internal$Model$hasSmallCaps, typefaces) ? 'small-caps' : 'normal')
					]);
				return A4($mdgriffith$elm_ui$Internal$Model$renderStyle, options, maybePseudo, '.' + name, families);
			case 3:
				var _class = rule.a;
				var prop = rule.b;
				var val = rule.c;
				return A4(
					$mdgriffith$elm_ui$Internal$Model$renderStyle,
					options,
					maybePseudo,
					'.' + _class,
					_List_fromArray(
						[
							A2($mdgriffith$elm_ui$Internal$Model$Property, prop, val)
						]));
			case 4:
				var _class = rule.a;
				var prop = rule.b;
				var color = rule.c;
				return A4(
					$mdgriffith$elm_ui$Internal$Model$renderStyle,
					options,
					maybePseudo,
					'.' + _class,
					_List_fromArray(
						[
							A2(
							$mdgriffith$elm_ui$Internal$Model$Property,
							prop,
							$mdgriffith$elm_ui$Internal$Model$formatColor(color))
						]));
			case 5:
				var cls = rule.a;
				var x = rule.b;
				var y = rule.c;
				var yPx = $elm$core$String$fromInt(y) + 'px';
				var xPx = $elm$core$String$fromInt(x) + 'px';
				var single = '.' + $mdgriffith$elm_ui$Internal$Style$classes.cX;
				var row = '.' + $mdgriffith$elm_ui$Internal$Style$classes.s;
				var wrappedRow = '.' + ($mdgriffith$elm_ui$Internal$Style$classes.aU + row);
				var right = '.' + $mdgriffith$elm_ui$Internal$Style$classes.aX;
				var paragraph = '.' + $mdgriffith$elm_ui$Internal$Style$classes.bj;
				var page = '.' + $mdgriffith$elm_ui$Internal$Style$classes.bi;
				var left = '.' + $mdgriffith$elm_ui$Internal$Style$classes.aW;
				var halfY = $elm$core$String$fromFloat(y / 2) + 'px';
				var halfX = $elm$core$String$fromFloat(x / 2) + 'px';
				var column = '.' + $mdgriffith$elm_ui$Internal$Style$classes.B;
				var _class = '.' + cls;
				var any = '.' + $mdgriffith$elm_ui$Internal$Style$classes.bS;
				return $elm$core$List$concat(
					_List_fromArray(
						[
							A4(
							$mdgriffith$elm_ui$Internal$Model$renderStyle,
							options,
							maybePseudo,
							_class + (row + (' > ' + (any + (' + ' + any)))),
							_List_fromArray(
								[
									A2($mdgriffith$elm_ui$Internal$Model$Property, 'margin-left', xPx)
								])),
							A4(
							$mdgriffith$elm_ui$Internal$Model$renderStyle,
							options,
							maybePseudo,
							_class + (wrappedRow + (' > ' + any)),
							_List_fromArray(
								[
									A2($mdgriffith$elm_ui$Internal$Model$Property, 'margin', halfY + (' ' + halfX))
								])),
							A4(
							$mdgriffith$elm_ui$Internal$Model$renderStyle,
							options,
							maybePseudo,
							_class + (column + (' > ' + (any + (' + ' + any)))),
							_List_fromArray(
								[
									A2($mdgriffith$elm_ui$Internal$Model$Property, 'margin-top', yPx)
								])),
							A4(
							$mdgriffith$elm_ui$Internal$Model$renderStyle,
							options,
							maybePseudo,
							_class + (page + (' > ' + (any + (' + ' + any)))),
							_List_fromArray(
								[
									A2($mdgriffith$elm_ui$Internal$Model$Property, 'margin-top', yPx)
								])),
							A4(
							$mdgriffith$elm_ui$Internal$Model$renderStyle,
							options,
							maybePseudo,
							_class + (page + (' > ' + left)),
							_List_fromArray(
								[
									A2($mdgriffith$elm_ui$Internal$Model$Property, 'margin-right', xPx)
								])),
							A4(
							$mdgriffith$elm_ui$Internal$Model$renderStyle,
							options,
							maybePseudo,
							_class + (page + (' > ' + right)),
							_List_fromArray(
								[
									A2($mdgriffith$elm_ui$Internal$Model$Property, 'margin-left', xPx)
								])),
							A4(
							$mdgriffith$elm_ui$Internal$Model$renderStyle,
							options,
							maybePseudo,
							_Utils_ap(_class, paragraph),
							_List_fromArray(
								[
									A2(
									$mdgriffith$elm_ui$Internal$Model$Property,
									'line-height',
									'calc(1em + ' + ($elm$core$String$fromInt(y) + 'px)'))
								])),
							A4(
							$mdgriffith$elm_ui$Internal$Model$renderStyle,
							options,
							maybePseudo,
							'textarea' + (any + _class),
							_List_fromArray(
								[
									A2(
									$mdgriffith$elm_ui$Internal$Model$Property,
									'line-height',
									'calc(1em + ' + ($elm$core$String$fromInt(y) + 'px)')),
									A2(
									$mdgriffith$elm_ui$Internal$Model$Property,
									'height',
									'calc(100% + ' + ($elm$core$String$fromInt(y) + 'px)'))
								])),
							A4(
							$mdgriffith$elm_ui$Internal$Model$renderStyle,
							options,
							maybePseudo,
							_class + (paragraph + (' > ' + left)),
							_List_fromArray(
								[
									A2($mdgriffith$elm_ui$Internal$Model$Property, 'margin-right', xPx)
								])),
							A4(
							$mdgriffith$elm_ui$Internal$Model$renderStyle,
							options,
							maybePseudo,
							_class + (paragraph + (' > ' + right)),
							_List_fromArray(
								[
									A2($mdgriffith$elm_ui$Internal$Model$Property, 'margin-left', xPx)
								])),
							A4(
							$mdgriffith$elm_ui$Internal$Model$renderStyle,
							options,
							maybePseudo,
							_class + (paragraph + '::after'),
							_List_fromArray(
								[
									A2($mdgriffith$elm_ui$Internal$Model$Property, 'content', '\'\''),
									A2($mdgriffith$elm_ui$Internal$Model$Property, 'display', 'block'),
									A2($mdgriffith$elm_ui$Internal$Model$Property, 'height', '0'),
									A2($mdgriffith$elm_ui$Internal$Model$Property, 'width', '0'),
									A2(
									$mdgriffith$elm_ui$Internal$Model$Property,
									'margin-top',
									$elm$core$String$fromInt((-1) * ((y / 2) | 0)) + 'px')
								])),
							A4(
							$mdgriffith$elm_ui$Internal$Model$renderStyle,
							options,
							maybePseudo,
							_class + (paragraph + '::before'),
							_List_fromArray(
								[
									A2($mdgriffith$elm_ui$Internal$Model$Property, 'content', '\'\''),
									A2($mdgriffith$elm_ui$Internal$Model$Property, 'display', 'block'),
									A2($mdgriffith$elm_ui$Internal$Model$Property, 'height', '0'),
									A2($mdgriffith$elm_ui$Internal$Model$Property, 'width', '0'),
									A2(
									$mdgriffith$elm_ui$Internal$Model$Property,
									'margin-bottom',
									$elm$core$String$fromInt((-1) * ((y / 2) | 0)) + 'px')
								]))
						]));
			case 7:
				var cls = rule.a;
				var top = rule.b;
				var right = rule.c;
				var bottom = rule.d;
				var left = rule.e;
				var _class = '.' + cls;
				return A4(
					$mdgriffith$elm_ui$Internal$Model$renderStyle,
					options,
					maybePseudo,
					_class,
					_List_fromArray(
						[
							A2(
							$mdgriffith$elm_ui$Internal$Model$Property,
							'padding',
							$elm$core$String$fromInt(top) + ('px ' + ($elm$core$String$fromInt(right) + ('px ' + ($elm$core$String$fromInt(bottom) + ('px ' + ($elm$core$String$fromInt(left) + 'px')))))))
						]));
			case 6:
				var cls = rule.a;
				var top = rule.b;
				var right = rule.c;
				var bottom = rule.d;
				var left = rule.e;
				var _class = '.' + cls;
				return A4(
					$mdgriffith$elm_ui$Internal$Model$renderStyle,
					options,
					maybePseudo,
					_class,
					_List_fromArray(
						[
							A2(
							$mdgriffith$elm_ui$Internal$Model$Property,
							'border-width',
							$elm$core$String$fromInt(top) + ('px ' + ($elm$core$String$fromInt(right) + ('px ' + ($elm$core$String$fromInt(bottom) + ('px ' + ($elm$core$String$fromInt(left) + 'px')))))))
						]));
			case 8:
				var template = rule.a;
				var toGridLengthHelper = F3(
					function (minimum, maximum, x) {
						toGridLengthHelper:
						while (true) {
							switch (x.$) {
								case 0:
									var px = x.a;
									return $elm$core$String$fromInt(px) + 'px';
								case 1:
									var _v2 = _Utils_Tuple2(minimum, maximum);
									if (_v2.a.$ === 1) {
										if (_v2.b.$ === 1) {
											var _v3 = _v2.a;
											var _v4 = _v2.b;
											return 'max-content';
										} else {
											var _v6 = _v2.a;
											var maxSize = _v2.b.a;
											return 'minmax(max-content, ' + ($elm$core$String$fromInt(maxSize) + 'px)');
										}
									} else {
										if (_v2.b.$ === 1) {
											var minSize = _v2.a.a;
											var _v5 = _v2.b;
											return 'minmax(' + ($elm$core$String$fromInt(minSize) + ('px, ' + 'max-content)'));
										} else {
											var minSize = _v2.a.a;
											var maxSize = _v2.b.a;
											return 'minmax(' + ($elm$core$String$fromInt(minSize) + ('px, ' + ($elm$core$String$fromInt(maxSize) + 'px)')));
										}
									}
								case 2:
									var i = x.a;
									var _v7 = _Utils_Tuple2(minimum, maximum);
									if (_v7.a.$ === 1) {
										if (_v7.b.$ === 1) {
											var _v8 = _v7.a;
											var _v9 = _v7.b;
											return $elm$core$String$fromInt(i) + 'fr';
										} else {
											var _v11 = _v7.a;
											var maxSize = _v7.b.a;
											return 'minmax(max-content, ' + ($elm$core$String$fromInt(maxSize) + 'px)');
										}
									} else {
										if (_v7.b.$ === 1) {
											var minSize = _v7.a.a;
											var _v10 = _v7.b;
											return 'minmax(' + ($elm$core$String$fromInt(minSize) + ('px, ' + ($elm$core$String$fromInt(i) + ('fr' + 'fr)'))));
										} else {
											var minSize = _v7.a.a;
											var maxSize = _v7.b.a;
											return 'minmax(' + ($elm$core$String$fromInt(minSize) + ('px, ' + ($elm$core$String$fromInt(maxSize) + 'px)')));
										}
									}
								case 3:
									var m = x.a;
									var len = x.b;
									var $temp$minimum = $elm$core$Maybe$Just(m),
										$temp$maximum = maximum,
										$temp$x = len;
									minimum = $temp$minimum;
									maximum = $temp$maximum;
									x = $temp$x;
									continue toGridLengthHelper;
								default:
									var m = x.a;
									var len = x.b;
									var $temp$minimum = minimum,
										$temp$maximum = $elm$core$Maybe$Just(m),
										$temp$x = len;
									minimum = $temp$minimum;
									maximum = $temp$maximum;
									x = $temp$x;
									continue toGridLengthHelper;
							}
						}
					});
				var toGridLength = function (x) {
					return A3(toGridLengthHelper, $elm$core$Maybe$Nothing, $elm$core$Maybe$Nothing, x);
				};
				var xSpacing = toGridLength(template.c_.a);
				var ySpacing = toGridLength(template.c_.b);
				var rows = function (x) {
					return 'grid-template-rows: ' + (x + ';');
				}(
					A2(
						$elm$core$String$join,
						' ',
						A2($elm$core$List$map, toGridLength, template.cR)));
				var msRows = function (x) {
					return '-ms-grid-rows: ' + (x + ';');
				}(
					A2(
						$elm$core$String$join,
						ySpacing,
						A2($elm$core$List$map, toGridLength, template.u)));
				var msColumns = function (x) {
					return '-ms-grid-columns: ' + (x + ';');
				}(
					A2(
						$elm$core$String$join,
						ySpacing,
						A2($elm$core$List$map, toGridLength, template.u)));
				var gapY = 'grid-row-gap:' + (toGridLength(template.c_.b) + ';');
				var gapX = 'grid-column-gap:' + (toGridLength(template.c_.a) + ';');
				var columns = function (x) {
					return 'grid-template-columns: ' + (x + ';');
				}(
					A2(
						$elm$core$String$join,
						' ',
						A2($elm$core$List$map, toGridLength, template.u)));
				var _class = '.grid-rows-' + (A2(
					$elm$core$String$join,
					'-',
					A2($elm$core$List$map, $mdgriffith$elm_ui$Internal$Model$lengthClassName, template.cR)) + ('-cols-' + (A2(
					$elm$core$String$join,
					'-',
					A2($elm$core$List$map, $mdgriffith$elm_ui$Internal$Model$lengthClassName, template.u)) + ('-space-x-' + ($mdgriffith$elm_ui$Internal$Model$lengthClassName(template.c_.a) + ('-space-y-' + $mdgriffith$elm_ui$Internal$Model$lengthClassName(template.c_.b)))))));
				var modernGrid = _class + ('{' + (columns + (rows + (gapX + (gapY + '}')))));
				var supports = '@supports (display:grid) {' + (modernGrid + '}');
				var base = _class + ('{' + (msColumns + (msRows + '}')));
				return _List_fromArray(
					[base, supports]);
			case 9:
				var position = rule.a;
				var msPosition = A2(
					$elm$core$String$join,
					' ',
					_List_fromArray(
						[
							'-ms-grid-row: ' + ($elm$core$String$fromInt(position.s) + ';'),
							'-ms-grid-row-span: ' + ($elm$core$String$fromInt(position.aG) + ';'),
							'-ms-grid-column: ' + ($elm$core$String$fromInt(position.cb) + ';'),
							'-ms-grid-column-span: ' + ($elm$core$String$fromInt(position.al) + ';')
						]));
				var modernPosition = A2(
					$elm$core$String$join,
					' ',
					_List_fromArray(
						[
							'grid-row: ' + ($elm$core$String$fromInt(position.s) + (' / ' + ($elm$core$String$fromInt(position.s + position.aG) + ';'))),
							'grid-column: ' + ($elm$core$String$fromInt(position.cb) + (' / ' + ($elm$core$String$fromInt(position.cb + position.al) + ';')))
						]));
				var _class = '.grid-pos-' + ($elm$core$String$fromInt(position.s) + ('-' + ($elm$core$String$fromInt(position.cb) + ('-' + ($elm$core$String$fromInt(position.al) + ('-' + $elm$core$String$fromInt(position.aG)))))));
				var modernGrid = _class + ('{' + (modernPosition + '}'));
				var supports = '@supports (display:grid) {' + (modernGrid + '}');
				var base = _class + ('{' + (msPosition + '}'));
				return _List_fromArray(
					[base, supports]);
			case 11:
				var _class = rule.a;
				var styles = rule.b;
				var renderPseudoRule = function (style) {
					return A3(
						$mdgriffith$elm_ui$Internal$Model$renderStyleRule,
						options,
						style,
						$elm$core$Maybe$Just(_class));
				};
				return A2($elm$core$List$concatMap, renderPseudoRule, styles);
			default:
				var transform = rule.a;
				var val = $mdgriffith$elm_ui$Internal$Model$transformValue(transform);
				var _class = $mdgriffith$elm_ui$Internal$Model$transformClass(transform);
				var _v12 = _Utils_Tuple2(_class, val);
				if ((!_v12.a.$) && (!_v12.b.$)) {
					var cls = _v12.a.a;
					var v = _v12.b.a;
					return A4(
						$mdgriffith$elm_ui$Internal$Model$renderStyle,
						options,
						maybePseudo,
						'.' + cls,
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Model$Property, 'transform', v)
							]));
				} else {
					return _List_Nil;
				}
		}
	});
var $mdgriffith$elm_ui$Internal$Model$encodeStyles = F2(
	function (options, stylesheet) {
		return $elm$json$Json$Encode$object(
			A2(
				$elm$core$List$map,
				function (style) {
					var styled = A3($mdgriffith$elm_ui$Internal$Model$renderStyleRule, options, style, $elm$core$Maybe$Nothing);
					return _Utils_Tuple2(
						$mdgriffith$elm_ui$Internal$Model$getStyleName(style),
						A2($elm$json$Json$Encode$list, $elm$json$Json$Encode$string, styled));
				},
				stylesheet));
	});
var $mdgriffith$elm_ui$Internal$Model$bracket = F2(
	function (selector, rules) {
		var renderPair = function (_v0) {
			var name = _v0.a;
			var val = _v0.b;
			return name + (': ' + (val + ';'));
		};
		return selector + (' {' + (A2(
			$elm$core$String$join,
			'',
			A2($elm$core$List$map, renderPair, rules)) + '}'));
	});
var $mdgriffith$elm_ui$Internal$Model$fontRule = F3(
	function (name, modifier, _v0) {
		var parentAdj = _v0.a;
		var textAdjustment = _v0.b;
		return _List_fromArray(
			[
				A2($mdgriffith$elm_ui$Internal$Model$bracket, '.' + (name + ('.' + (modifier + (', ' + ('.' + (name + (' .' + modifier))))))), parentAdj),
				A2($mdgriffith$elm_ui$Internal$Model$bracket, '.' + (name + ('.' + (modifier + ('> .' + ($mdgriffith$elm_ui$Internal$Style$classes.c6 + (', .' + (name + (' .' + (modifier + (' > .' + $mdgriffith$elm_ui$Internal$Style$classes.c6)))))))))), textAdjustment)
			]);
	});
var $mdgriffith$elm_ui$Internal$Model$renderFontAdjustmentRule = F3(
	function (fontToAdjust, _v0, otherFontName) {
		var full = _v0.a;
		var capital = _v0.b;
		var name = _Utils_eq(fontToAdjust, otherFontName) ? fontToAdjust : (otherFontName + (' .' + fontToAdjust));
		return A2(
			$elm$core$String$join,
			' ',
			_Utils_ap(
				A3($mdgriffith$elm_ui$Internal$Model$fontRule, name, $mdgriffith$elm_ui$Internal$Style$classes.cY, capital),
				A3($mdgriffith$elm_ui$Internal$Model$fontRule, name, $mdgriffith$elm_ui$Internal$Style$classes.cm, full)));
	});
var $mdgriffith$elm_ui$Internal$Model$renderNullAdjustmentRule = F2(
	function (fontToAdjust, otherFontName) {
		var name = _Utils_eq(fontToAdjust, otherFontName) ? fontToAdjust : (otherFontName + (' .' + fontToAdjust));
		return A2(
			$elm$core$String$join,
			' ',
			_List_fromArray(
				[
					A2(
					$mdgriffith$elm_ui$Internal$Model$bracket,
					'.' + (name + ('.' + ($mdgriffith$elm_ui$Internal$Style$classes.cY + (', ' + ('.' + (name + (' .' + $mdgriffith$elm_ui$Internal$Style$classes.cY))))))),
					_List_fromArray(
						[
							_Utils_Tuple2('line-height', '1')
						])),
					A2(
					$mdgriffith$elm_ui$Internal$Model$bracket,
					'.' + (name + ('.' + ($mdgriffith$elm_ui$Internal$Style$classes.cY + ('> .' + ($mdgriffith$elm_ui$Internal$Style$classes.c6 + (', .' + (name + (' .' + ($mdgriffith$elm_ui$Internal$Style$classes.cY + (' > .' + $mdgriffith$elm_ui$Internal$Style$classes.c6)))))))))),
					_List_fromArray(
						[
							_Utils_Tuple2('vertical-align', '0'),
							_Utils_Tuple2('line-height', '1')
						]))
				]));
	});
var $mdgriffith$elm_ui$Internal$Model$adjust = F3(
	function (size, height, vertical) {
		return {aG: height / size, Z: size, bA: vertical};
	});
var $elm$core$List$filter = F2(
	function (isGood, list) {
		return A3(
			$elm$core$List$foldr,
			F2(
				function (x, xs) {
					return isGood(x) ? A2($elm$core$List$cons, x, xs) : xs;
				}),
			_List_Nil,
			list);
	});
var $elm$core$List$maximum = function (list) {
	if (list.b) {
		var x = list.a;
		var xs = list.b;
		return $elm$core$Maybe$Just(
			A3($elm$core$List$foldl, $elm$core$Basics$max, x, xs));
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $elm$core$List$minimum = function (list) {
	if (list.b) {
		var x = list.a;
		var xs = list.b;
		return $elm$core$Maybe$Just(
			A3($elm$core$List$foldl, $elm$core$Basics$min, x, xs));
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $elm$core$Basics$neq = _Utils_notEqual;
var $mdgriffith$elm_ui$Internal$Model$convertAdjustment = function (adjustment) {
	var lines = _List_fromArray(
		[adjustment.b5, adjustment.bV, adjustment.cf, adjustment.cB]);
	var lineHeight = 1.5;
	var normalDescender = (lineHeight - 1) / 2;
	var oldMiddle = lineHeight / 2;
	var descender = A2(
		$elm$core$Maybe$withDefault,
		adjustment.cf,
		$elm$core$List$minimum(lines));
	var newBaseline = A2(
		$elm$core$Maybe$withDefault,
		adjustment.bV,
		$elm$core$List$minimum(
			A2(
				$elm$core$List$filter,
				function (x) {
					return !_Utils_eq(x, descender);
				},
				lines)));
	var base = lineHeight;
	var ascender = A2(
		$elm$core$Maybe$withDefault,
		adjustment.b5,
		$elm$core$List$maximum(lines));
	var capitalSize = 1 / (ascender - newBaseline);
	var capitalVertical = 1 - ascender;
	var fullSize = 1 / (ascender - descender);
	var fullVertical = 1 - ascender;
	var newCapitalMiddle = ((ascender - newBaseline) / 2) + newBaseline;
	var newFullMiddle = ((ascender - descender) / 2) + descender;
	return {
		b5: A3($mdgriffith$elm_ui$Internal$Model$adjust, capitalSize, ascender - newBaseline, capitalVertical),
		a3: A3($mdgriffith$elm_ui$Internal$Model$adjust, fullSize, ascender - descender, fullVertical)
	};
};
var $mdgriffith$elm_ui$Internal$Model$fontAdjustmentRules = function (converted) {
	return _Utils_Tuple2(
		_List_fromArray(
			[
				_Utils_Tuple2('display', 'block')
			]),
		_List_fromArray(
			[
				_Utils_Tuple2('display', 'inline-block'),
				_Utils_Tuple2(
				'line-height',
				$elm$core$String$fromFloat(converted.aG)),
				_Utils_Tuple2(
				'vertical-align',
				$elm$core$String$fromFloat(converted.bA) + 'em'),
				_Utils_Tuple2(
				'font-size',
				$elm$core$String$fromFloat(converted.Z) + 'em')
			]));
};
var $mdgriffith$elm_ui$Internal$Model$typefaceAdjustment = function (typefaces) {
	return A3(
		$elm$core$List$foldl,
		F2(
			function (face, found) {
				if (found.$ === 1) {
					if (face.$ === 5) {
						var _with = face.a;
						var _v2 = _with.bI;
						if (_v2.$ === 1) {
							return found;
						} else {
							var adjustment = _v2.a;
							return $elm$core$Maybe$Just(
								_Utils_Tuple2(
									$mdgriffith$elm_ui$Internal$Model$fontAdjustmentRules(
										function ($) {
											return $.a3;
										}(
											$mdgriffith$elm_ui$Internal$Model$convertAdjustment(adjustment))),
									$mdgriffith$elm_ui$Internal$Model$fontAdjustmentRules(
										function ($) {
											return $.b5;
										}(
											$mdgriffith$elm_ui$Internal$Model$convertAdjustment(adjustment)))));
						}
					} else {
						return found;
					}
				} else {
					return found;
				}
			}),
		$elm$core$Maybe$Nothing,
		typefaces);
};
var $mdgriffith$elm_ui$Internal$Model$renderTopLevelValues = function (rules) {
	var withImport = function (font) {
		if (font.$ === 4) {
			var url = font.b;
			return $elm$core$Maybe$Just('@import url(\'' + (url + '\');'));
		} else {
			return $elm$core$Maybe$Nothing;
		}
	};
	var fontImports = function (_v2) {
		var name = _v2.a;
		var typefaces = _v2.b;
		var imports = A2(
			$elm$core$String$join,
			'\n',
			A2($elm$core$List$filterMap, withImport, typefaces));
		return imports;
	};
	var allNames = A2($elm$core$List$map, $elm$core$Tuple$first, rules);
	var fontAdjustments = function (_v1) {
		var name = _v1.a;
		var typefaces = _v1.b;
		var _v0 = $mdgriffith$elm_ui$Internal$Model$typefaceAdjustment(typefaces);
		if (_v0.$ === 1) {
			return A2(
				$elm$core$String$join,
				'',
				A2(
					$elm$core$List$map,
					$mdgriffith$elm_ui$Internal$Model$renderNullAdjustmentRule(name),
					allNames));
		} else {
			var adjustment = _v0.a;
			return A2(
				$elm$core$String$join,
				'',
				A2(
					$elm$core$List$map,
					A2($mdgriffith$elm_ui$Internal$Model$renderFontAdjustmentRule, name, adjustment),
					allNames));
		}
	};
	return _Utils_ap(
		A2(
			$elm$core$String$join,
			'\n',
			A2($elm$core$List$map, fontImports, rules)),
		A2(
			$elm$core$String$join,
			'\n',
			A2($elm$core$List$map, fontAdjustments, rules)));
};
var $mdgriffith$elm_ui$Internal$Model$topLevelValue = function (rule) {
	if (rule.$ === 1) {
		var name = rule.a;
		var typefaces = rule.b;
		return $elm$core$Maybe$Just(
			_Utils_Tuple2(name, typefaces));
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $mdgriffith$elm_ui$Internal$Model$toStyleSheetString = F2(
	function (options, stylesheet) {
		var combine = F2(
			function (style, rendered) {
				return {
					av: _Utils_ap(
						rendered.av,
						A3($mdgriffith$elm_ui$Internal$Model$renderStyleRule, options, style, $elm$core$Maybe$Nothing)),
					ai: function () {
						var _v1 = $mdgriffith$elm_ui$Internal$Model$topLevelValue(style);
						if (_v1.$ === 1) {
							return rendered.ai;
						} else {
							var topLevel = _v1.a;
							return A2($elm$core$List$cons, topLevel, rendered.ai);
						}
					}()
				};
			});
		var _v0 = A3(
			$elm$core$List$foldl,
			combine,
			{av: _List_Nil, ai: _List_Nil},
			stylesheet);
		var topLevel = _v0.ai;
		var rules = _v0.av;
		return _Utils_ap(
			$mdgriffith$elm_ui$Internal$Model$renderTopLevelValues(topLevel),
			$elm$core$String$concat(rules));
	});
var $mdgriffith$elm_ui$Internal$Model$toStyleSheet = F2(
	function (options, styleSheet) {
		var _v0 = options.cC;
		switch (_v0) {
			case 0:
				return A3(
					$elm$virtual_dom$VirtualDom$node,
					'div',
					_List_Nil,
					_List_fromArray(
						[
							A3(
							$elm$virtual_dom$VirtualDom$node,
							'style',
							_List_Nil,
							_List_fromArray(
								[
									$elm$virtual_dom$VirtualDom$text(
									A2($mdgriffith$elm_ui$Internal$Model$toStyleSheetString, options, styleSheet))
								]))
						]));
			case 1:
				return A3(
					$elm$virtual_dom$VirtualDom$node,
					'div',
					_List_Nil,
					_List_fromArray(
						[
							A3(
							$elm$virtual_dom$VirtualDom$node,
							'style',
							_List_Nil,
							_List_fromArray(
								[
									$elm$virtual_dom$VirtualDom$text(
									A2($mdgriffith$elm_ui$Internal$Model$toStyleSheetString, options, styleSheet))
								]))
						]));
			default:
				return A3(
					$elm$virtual_dom$VirtualDom$node,
					'elm-ui-rules',
					_List_fromArray(
						[
							A2(
							$elm$virtual_dom$VirtualDom$property,
							'rules',
							A2($mdgriffith$elm_ui$Internal$Model$encodeStyles, options, styleSheet))
						]),
					_List_Nil);
		}
	});
var $mdgriffith$elm_ui$Internal$Model$embedKeyed = F4(
	function (_static, opts, styles, children) {
		var dynamicStyleSheet = A2(
			$mdgriffith$elm_ui$Internal$Model$toStyleSheet,
			opts,
			A3(
				$elm$core$List$foldl,
				$mdgriffith$elm_ui$Internal$Model$reduceStyles,
				_Utils_Tuple2(
					$elm$core$Set$empty,
					$mdgriffith$elm_ui$Internal$Model$renderFocusStyle(opts.cl)),
				styles).b);
		return _static ? A2(
			$elm$core$List$cons,
			_Utils_Tuple2(
				'static-stylesheet',
				$mdgriffith$elm_ui$Internal$Model$staticRoot(opts)),
			A2(
				$elm$core$List$cons,
				_Utils_Tuple2('dynamic-stylesheet', dynamicStyleSheet),
				children)) : A2(
			$elm$core$List$cons,
			_Utils_Tuple2('dynamic-stylesheet', dynamicStyleSheet),
			children);
	});
var $mdgriffith$elm_ui$Internal$Model$embedWith = F4(
	function (_static, opts, styles, children) {
		var dynamicStyleSheet = A2(
			$mdgriffith$elm_ui$Internal$Model$toStyleSheet,
			opts,
			A3(
				$elm$core$List$foldl,
				$mdgriffith$elm_ui$Internal$Model$reduceStyles,
				_Utils_Tuple2(
					$elm$core$Set$empty,
					$mdgriffith$elm_ui$Internal$Model$renderFocusStyle(opts.cl)),
				styles).b);
		return _static ? A2(
			$elm$core$List$cons,
			$mdgriffith$elm_ui$Internal$Model$staticRoot(opts),
			A2($elm$core$List$cons, dynamicStyleSheet, children)) : A2($elm$core$List$cons, dynamicStyleSheet, children);
	});
var $mdgriffith$elm_ui$Internal$Flag$heightBetween = $mdgriffith$elm_ui$Internal$Flag$flag(45);
var $mdgriffith$elm_ui$Internal$Flag$heightFill = $mdgriffith$elm_ui$Internal$Flag$flag(37);
var $elm$virtual_dom$VirtualDom$keyedNode = function (tag) {
	return _VirtualDom_keyedNode(
		_VirtualDom_noScript(tag));
};
var $elm$core$Basics$not = _Basics_not;
var $elm$html$Html$p = _VirtualDom_node('p');
var $mdgriffith$elm_ui$Internal$Flag$present = F2(
	function (myFlag, _v0) {
		var fieldOne = _v0.a;
		var fieldTwo = _v0.b;
		if (!myFlag.$) {
			var first = myFlag.a;
			return _Utils_eq(first & fieldOne, first);
		} else {
			var second = myFlag.a;
			return _Utils_eq(second & fieldTwo, second);
		}
	});
var $elm$html$Html$s = _VirtualDom_node('s');
var $elm$html$Html$u = _VirtualDom_node('u');
var $mdgriffith$elm_ui$Internal$Flag$widthBetween = $mdgriffith$elm_ui$Internal$Flag$flag(44);
var $mdgriffith$elm_ui$Internal$Flag$widthFill = $mdgriffith$elm_ui$Internal$Flag$flag(39);
var $mdgriffith$elm_ui$Internal$Model$finalizeNode = F6(
	function (has, node, attributes, children, embedMode, parentContext) {
		var createNode = F2(
			function (nodeName, attrs) {
				if (children.$ === 1) {
					var keyed = children.a;
					return A3(
						$elm$virtual_dom$VirtualDom$keyedNode,
						nodeName,
						attrs,
						function () {
							switch (embedMode.$) {
								case 0:
									return keyed;
								case 2:
									var opts = embedMode.a;
									var styles = embedMode.b;
									return A4($mdgriffith$elm_ui$Internal$Model$embedKeyed, false, opts, styles, keyed);
								default:
									var opts = embedMode.a;
									var styles = embedMode.b;
									return A4($mdgriffith$elm_ui$Internal$Model$embedKeyed, true, opts, styles, keyed);
							}
						}());
				} else {
					var unkeyed = children.a;
					return A2(
						function () {
							switch (nodeName) {
								case 'div':
									return $elm$html$Html$div;
								case 'p':
									return $elm$html$Html$p;
								default:
									return $elm$virtual_dom$VirtualDom$node(nodeName);
							}
						}(),
						attrs,
						function () {
							switch (embedMode.$) {
								case 0:
									return unkeyed;
								case 2:
									var opts = embedMode.a;
									var styles = embedMode.b;
									return A4($mdgriffith$elm_ui$Internal$Model$embedWith, false, opts, styles, unkeyed);
								default:
									var opts = embedMode.a;
									var styles = embedMode.b;
									return A4($mdgriffith$elm_ui$Internal$Model$embedWith, true, opts, styles, unkeyed);
							}
						}());
				}
			});
		var html = function () {
			switch (node.$) {
				case 0:
					return A2(createNode, 'div', attributes);
				case 1:
					var nodeName = node.a;
					return A2(createNode, nodeName, attributes);
				default:
					var nodeName = node.a;
					var internal = node.b;
					return A3(
						$elm$virtual_dom$VirtualDom$node,
						nodeName,
						attributes,
						_List_fromArray(
							[
								A2(
								createNode,
								internal,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$class($mdgriffith$elm_ui$Internal$Style$classes.bS + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.cX))
									]))
							]));
			}
		}();
		switch (parentContext) {
			case 0:
				return (A2($mdgriffith$elm_ui$Internal$Flag$present, $mdgriffith$elm_ui$Internal$Flag$widthFill, has) && (!A2($mdgriffith$elm_ui$Internal$Flag$present, $mdgriffith$elm_ui$Internal$Flag$widthBetween, has))) ? html : (A2($mdgriffith$elm_ui$Internal$Flag$present, $mdgriffith$elm_ui$Internal$Flag$alignRight, has) ? A2(
					$elm$html$Html$u,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class(
							A2(
								$elm$core$String$join,
								' ',
								_List_fromArray(
									[$mdgriffith$elm_ui$Internal$Style$classes.bS, $mdgriffith$elm_ui$Internal$Style$classes.cX, $mdgriffith$elm_ui$Internal$Style$classes.ap, $mdgriffith$elm_ui$Internal$Style$classes.C, $mdgriffith$elm_ui$Internal$Style$classes.bP])))
						]),
					_List_fromArray(
						[html])) : (A2($mdgriffith$elm_ui$Internal$Flag$present, $mdgriffith$elm_ui$Internal$Flag$centerX, has) ? A2(
					$elm$html$Html$s,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class(
							A2(
								$elm$core$String$join,
								' ',
								_List_fromArray(
									[$mdgriffith$elm_ui$Internal$Style$classes.bS, $mdgriffith$elm_ui$Internal$Style$classes.cX, $mdgriffith$elm_ui$Internal$Style$classes.ap, $mdgriffith$elm_ui$Internal$Style$classes.C, $mdgriffith$elm_ui$Internal$Style$classes.bN])))
						]),
					_List_fromArray(
						[html])) : html));
			case 1:
				return (A2($mdgriffith$elm_ui$Internal$Flag$present, $mdgriffith$elm_ui$Internal$Flag$heightFill, has) && (!A2($mdgriffith$elm_ui$Internal$Flag$present, $mdgriffith$elm_ui$Internal$Flag$heightBetween, has))) ? html : (A2($mdgriffith$elm_ui$Internal$Flag$present, $mdgriffith$elm_ui$Internal$Flag$centerY, has) ? A2(
					$elm$html$Html$s,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class(
							A2(
								$elm$core$String$join,
								' ',
								_List_fromArray(
									[$mdgriffith$elm_ui$Internal$Style$classes.bS, $mdgriffith$elm_ui$Internal$Style$classes.cX, $mdgriffith$elm_ui$Internal$Style$classes.ap, $mdgriffith$elm_ui$Internal$Style$classes.bO])))
						]),
					_List_fromArray(
						[html])) : (A2($mdgriffith$elm_ui$Internal$Flag$present, $mdgriffith$elm_ui$Internal$Flag$alignBottom, has) ? A2(
					$elm$html$Html$u,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class(
							A2(
								$elm$core$String$join,
								' ',
								_List_fromArray(
									[$mdgriffith$elm_ui$Internal$Style$classes.bS, $mdgriffith$elm_ui$Internal$Style$classes.cX, $mdgriffith$elm_ui$Internal$Style$classes.ap, $mdgriffith$elm_ui$Internal$Style$classes.bM])))
						]),
					_List_fromArray(
						[html])) : html));
			default:
				return html;
		}
	});
var $elm$html$Html$text = $elm$virtual_dom$VirtualDom$text;
var $mdgriffith$elm_ui$Internal$Model$textElementClasses = $mdgriffith$elm_ui$Internal$Style$classes.bS + (' ' + ($mdgriffith$elm_ui$Internal$Style$classes.c6 + (' ' + ($mdgriffith$elm_ui$Internal$Style$classes.aS + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.aH)))));
var $mdgriffith$elm_ui$Internal$Model$textElement = function (str) {
	return A2(
		$elm$html$Html$div,
		_List_fromArray(
			[
				$elm$html$Html$Attributes$class($mdgriffith$elm_ui$Internal$Model$textElementClasses)
			]),
		_List_fromArray(
			[
				$elm$html$Html$text(str)
			]));
};
var $mdgriffith$elm_ui$Internal$Model$textElementFillClasses = $mdgriffith$elm_ui$Internal$Style$classes.bS + (' ' + ($mdgriffith$elm_ui$Internal$Style$classes.c6 + (' ' + ($mdgriffith$elm_ui$Internal$Style$classes.aT + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.aI)))));
var $mdgriffith$elm_ui$Internal$Model$textElementFill = function (str) {
	return A2(
		$elm$html$Html$div,
		_List_fromArray(
			[
				$elm$html$Html$Attributes$class($mdgriffith$elm_ui$Internal$Model$textElementFillClasses)
			]),
		_List_fromArray(
			[
				$elm$html$Html$text(str)
			]));
};
var $mdgriffith$elm_ui$Internal$Model$createElement = F3(
	function (context, children, rendered) {
		var gatherKeyed = F2(
			function (_v8, _v9) {
				var key = _v8.a;
				var child = _v8.b;
				var htmls = _v9.a;
				var existingStyles = _v9.b;
				switch (child.$) {
					case 0:
						var html = child.a;
						return _Utils_eq(context, $mdgriffith$elm_ui$Internal$Model$asParagraph) ? _Utils_Tuple2(
							A2(
								$elm$core$List$cons,
								_Utils_Tuple2(
									key,
									html(context)),
								htmls),
							existingStyles) : _Utils_Tuple2(
							A2(
								$elm$core$List$cons,
								_Utils_Tuple2(
									key,
									html(context)),
								htmls),
							existingStyles);
					case 1:
						var styled = child.a;
						return _Utils_eq(context, $mdgriffith$elm_ui$Internal$Model$asParagraph) ? _Utils_Tuple2(
							A2(
								$elm$core$List$cons,
								_Utils_Tuple2(
									key,
									A2(styled.cq, $mdgriffith$elm_ui$Internal$Model$NoStyleSheet, context)),
								htmls),
							$elm$core$List$isEmpty(existingStyles) ? styled.c4 : _Utils_ap(styled.c4, existingStyles)) : _Utils_Tuple2(
							A2(
								$elm$core$List$cons,
								_Utils_Tuple2(
									key,
									A2(styled.cq, $mdgriffith$elm_ui$Internal$Model$NoStyleSheet, context)),
								htmls),
							$elm$core$List$isEmpty(existingStyles) ? styled.c4 : _Utils_ap(styled.c4, existingStyles));
					case 2:
						var str = child.a;
						return _Utils_Tuple2(
							A2(
								$elm$core$List$cons,
								_Utils_Tuple2(
									key,
									_Utils_eq(context, $mdgriffith$elm_ui$Internal$Model$asEl) ? $mdgriffith$elm_ui$Internal$Model$textElementFill(str) : $mdgriffith$elm_ui$Internal$Model$textElement(str)),
								htmls),
							existingStyles);
					default:
						return _Utils_Tuple2(htmls, existingStyles);
				}
			});
		var gather = F2(
			function (child, _v6) {
				var htmls = _v6.a;
				var existingStyles = _v6.b;
				switch (child.$) {
					case 0:
						var html = child.a;
						return _Utils_eq(context, $mdgriffith$elm_ui$Internal$Model$asParagraph) ? _Utils_Tuple2(
							A2(
								$elm$core$List$cons,
								html(context),
								htmls),
							existingStyles) : _Utils_Tuple2(
							A2(
								$elm$core$List$cons,
								html(context),
								htmls),
							existingStyles);
					case 1:
						var styled = child.a;
						return _Utils_eq(context, $mdgriffith$elm_ui$Internal$Model$asParagraph) ? _Utils_Tuple2(
							A2(
								$elm$core$List$cons,
								A2(styled.cq, $mdgriffith$elm_ui$Internal$Model$NoStyleSheet, context),
								htmls),
							$elm$core$List$isEmpty(existingStyles) ? styled.c4 : _Utils_ap(styled.c4, existingStyles)) : _Utils_Tuple2(
							A2(
								$elm$core$List$cons,
								A2(styled.cq, $mdgriffith$elm_ui$Internal$Model$NoStyleSheet, context),
								htmls),
							$elm$core$List$isEmpty(existingStyles) ? styled.c4 : _Utils_ap(styled.c4, existingStyles));
					case 2:
						var str = child.a;
						return _Utils_Tuple2(
							A2(
								$elm$core$List$cons,
								_Utils_eq(context, $mdgriffith$elm_ui$Internal$Model$asEl) ? $mdgriffith$elm_ui$Internal$Model$textElementFill(str) : $mdgriffith$elm_ui$Internal$Model$textElement(str),
								htmls),
							existingStyles);
					default:
						return _Utils_Tuple2(htmls, existingStyles);
				}
			});
		if (children.$ === 1) {
			var keyedChildren = children.a;
			var _v1 = A3(
				$elm$core$List$foldr,
				gatherKeyed,
				_Utils_Tuple2(_List_Nil, _List_Nil),
				keyedChildren);
			var keyed = _v1.a;
			var styles = _v1.b;
			var newStyles = $elm$core$List$isEmpty(styles) ? rendered.c4 : _Utils_ap(rendered.c4, styles);
			if (!newStyles.b) {
				return $mdgriffith$elm_ui$Internal$Model$Unstyled(
					A5(
						$mdgriffith$elm_ui$Internal$Model$finalizeNode,
						rendered.K,
						rendered.L,
						rendered.G,
						$mdgriffith$elm_ui$Internal$Model$Keyed(
							A3($mdgriffith$elm_ui$Internal$Model$addKeyedChildren, 'nearby-element-pls', keyed, rendered.H)),
						$mdgriffith$elm_ui$Internal$Model$NoStyleSheet));
			} else {
				var allStyles = newStyles;
				return $mdgriffith$elm_ui$Internal$Model$Styled(
					{
						cq: A4(
							$mdgriffith$elm_ui$Internal$Model$finalizeNode,
							rendered.K,
							rendered.L,
							rendered.G,
							$mdgriffith$elm_ui$Internal$Model$Keyed(
								A3($mdgriffith$elm_ui$Internal$Model$addKeyedChildren, 'nearby-element-pls', keyed, rendered.H))),
						c4: allStyles
					});
			}
		} else {
			var unkeyedChildren = children.a;
			var _v3 = A3(
				$elm$core$List$foldr,
				gather,
				_Utils_Tuple2(_List_Nil, _List_Nil),
				unkeyedChildren);
			var unkeyed = _v3.a;
			var styles = _v3.b;
			var newStyles = $elm$core$List$isEmpty(styles) ? rendered.c4 : _Utils_ap(rendered.c4, styles);
			if (!newStyles.b) {
				return $mdgriffith$elm_ui$Internal$Model$Unstyled(
					A5(
						$mdgriffith$elm_ui$Internal$Model$finalizeNode,
						rendered.K,
						rendered.L,
						rendered.G,
						$mdgriffith$elm_ui$Internal$Model$Unkeyed(
							A2($mdgriffith$elm_ui$Internal$Model$addChildren, unkeyed, rendered.H)),
						$mdgriffith$elm_ui$Internal$Model$NoStyleSheet));
			} else {
				var allStyles = newStyles;
				return $mdgriffith$elm_ui$Internal$Model$Styled(
					{
						cq: A4(
							$mdgriffith$elm_ui$Internal$Model$finalizeNode,
							rendered.K,
							rendered.L,
							rendered.G,
							$mdgriffith$elm_ui$Internal$Model$Unkeyed(
								A2($mdgriffith$elm_ui$Internal$Model$addChildren, unkeyed, rendered.H))),
						c4: allStyles
					});
			}
		}
	});
var $mdgriffith$elm_ui$Internal$Model$Single = F3(
	function (a, b, c) {
		return {$: 3, a: a, b: b, c: c};
	});
var $mdgriffith$elm_ui$Internal$Model$Transform = function (a) {
	return {$: 10, a: a};
};
var $mdgriffith$elm_ui$Internal$Flag$Field = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $elm$core$Bitwise$or = _Bitwise_or;
var $mdgriffith$elm_ui$Internal$Flag$add = F2(
	function (myFlag, _v0) {
		var one = _v0.a;
		var two = _v0.b;
		if (!myFlag.$) {
			var first = myFlag.a;
			return A2($mdgriffith$elm_ui$Internal$Flag$Field, first | one, two);
		} else {
			var second = myFlag.a;
			return A2($mdgriffith$elm_ui$Internal$Flag$Field, one, second | two);
		}
	});
var $mdgriffith$elm_ui$Internal$Model$ChildrenBehind = function (a) {
	return {$: 1, a: a};
};
var $mdgriffith$elm_ui$Internal$Model$ChildrenBehindAndInFront = F2(
	function (a, b) {
		return {$: 3, a: a, b: b};
	});
var $mdgriffith$elm_ui$Internal$Model$ChildrenInFront = function (a) {
	return {$: 2, a: a};
};
var $mdgriffith$elm_ui$Internal$Model$nearbyElement = F2(
	function (location, elem) {
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class(
					function () {
						switch (location) {
							case 0:
								return A2(
									$elm$core$String$join,
									' ',
									_List_fromArray(
										[$mdgriffith$elm_ui$Internal$Style$classes.X, $mdgriffith$elm_ui$Internal$Style$classes.cX, $mdgriffith$elm_ui$Internal$Style$classes.bH]));
							case 1:
								return A2(
									$elm$core$String$join,
									' ',
									_List_fromArray(
										[$mdgriffith$elm_ui$Internal$Style$classes.X, $mdgriffith$elm_ui$Internal$Style$classes.cX, $mdgriffith$elm_ui$Internal$Style$classes.bX]));
							case 2:
								return A2(
									$elm$core$String$join,
									' ',
									_List_fromArray(
										[$mdgriffith$elm_ui$Internal$Style$classes.X, $mdgriffith$elm_ui$Internal$Style$classes.cX, $mdgriffith$elm_ui$Internal$Style$classes.cH]));
							case 3:
								return A2(
									$elm$core$String$join,
									' ',
									_List_fromArray(
										[$mdgriffith$elm_ui$Internal$Style$classes.X, $mdgriffith$elm_ui$Internal$Style$classes.cX, $mdgriffith$elm_ui$Internal$Style$classes.cF]));
							case 4:
								return A2(
									$elm$core$String$join,
									' ',
									_List_fromArray(
										[$mdgriffith$elm_ui$Internal$Style$classes.X, $mdgriffith$elm_ui$Internal$Style$classes.cX, $mdgriffith$elm_ui$Internal$Style$classes.ct]));
							default:
								return A2(
									$elm$core$String$join,
									' ',
									_List_fromArray(
										[$mdgriffith$elm_ui$Internal$Style$classes.X, $mdgriffith$elm_ui$Internal$Style$classes.cX, $mdgriffith$elm_ui$Internal$Style$classes.bW]));
						}
					}())
				]),
			_List_fromArray(
				[
					function () {
					switch (elem.$) {
						case 3:
							return $elm$virtual_dom$VirtualDom$text('');
						case 2:
							var str = elem.a;
							return $mdgriffith$elm_ui$Internal$Model$textElement(str);
						case 0:
							var html = elem.a;
							return html($mdgriffith$elm_ui$Internal$Model$asEl);
						default:
							var styled = elem.a;
							return A2(styled.cq, $mdgriffith$elm_ui$Internal$Model$NoStyleSheet, $mdgriffith$elm_ui$Internal$Model$asEl);
					}
				}()
				]));
	});
var $mdgriffith$elm_ui$Internal$Model$addNearbyElement = F3(
	function (location, elem, existing) {
		var nearby = A2($mdgriffith$elm_ui$Internal$Model$nearbyElement, location, elem);
		switch (existing.$) {
			case 0:
				if (location === 5) {
					return $mdgriffith$elm_ui$Internal$Model$ChildrenBehind(
						_List_fromArray(
							[nearby]));
				} else {
					return $mdgriffith$elm_ui$Internal$Model$ChildrenInFront(
						_List_fromArray(
							[nearby]));
				}
			case 1:
				var existingBehind = existing.a;
				if (location === 5) {
					return $mdgriffith$elm_ui$Internal$Model$ChildrenBehind(
						A2($elm$core$List$cons, nearby, existingBehind));
				} else {
					return A2(
						$mdgriffith$elm_ui$Internal$Model$ChildrenBehindAndInFront,
						existingBehind,
						_List_fromArray(
							[nearby]));
				}
			case 2:
				var existingInFront = existing.a;
				if (location === 5) {
					return A2(
						$mdgriffith$elm_ui$Internal$Model$ChildrenBehindAndInFront,
						_List_fromArray(
							[nearby]),
						existingInFront);
				} else {
					return $mdgriffith$elm_ui$Internal$Model$ChildrenInFront(
						A2($elm$core$List$cons, nearby, existingInFront));
				}
			default:
				var existingBehind = existing.a;
				var existingInFront = existing.b;
				if (location === 5) {
					return A2(
						$mdgriffith$elm_ui$Internal$Model$ChildrenBehindAndInFront,
						A2($elm$core$List$cons, nearby, existingBehind),
						existingInFront);
				} else {
					return A2(
						$mdgriffith$elm_ui$Internal$Model$ChildrenBehindAndInFront,
						existingBehind,
						A2($elm$core$List$cons, nearby, existingInFront));
				}
		}
	});
var $mdgriffith$elm_ui$Internal$Model$Embedded = F2(
	function (a, b) {
		return {$: 2, a: a, b: b};
	});
var $mdgriffith$elm_ui$Internal$Model$NodeName = function (a) {
	return {$: 1, a: a};
};
var $mdgriffith$elm_ui$Internal$Model$addNodeName = F2(
	function (newNode, old) {
		switch (old.$) {
			case 0:
				return $mdgriffith$elm_ui$Internal$Model$NodeName(newNode);
			case 1:
				var name = old.a;
				return A2($mdgriffith$elm_ui$Internal$Model$Embedded, name, newNode);
			default:
				var x = old.a;
				var y = old.b;
				return A2($mdgriffith$elm_ui$Internal$Model$Embedded, x, y);
		}
	});
var $mdgriffith$elm_ui$Internal$Model$alignXName = function (align) {
	switch (align) {
		case 0:
			return $mdgriffith$elm_ui$Internal$Style$classes.aA + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.aW);
		case 2:
			return $mdgriffith$elm_ui$Internal$Style$classes.aA + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.aX);
		default:
			return $mdgriffith$elm_ui$Internal$Style$classes.aA + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.bK);
	}
};
var $mdgriffith$elm_ui$Internal$Model$alignYName = function (align) {
	switch (align) {
		case 0:
			return $mdgriffith$elm_ui$Internal$Style$classes.aB + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.bQ);
		case 2:
			return $mdgriffith$elm_ui$Internal$Style$classes.aB + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.bJ);
		default:
			return $mdgriffith$elm_ui$Internal$Style$classes.aB + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.bL);
	}
};
var $elm$virtual_dom$VirtualDom$attribute = F2(
	function (key, value) {
		return A2(
			_VirtualDom_attribute,
			_VirtualDom_noOnOrFormAction(key),
			_VirtualDom_noJavaScriptOrHtmlUri(value));
	});
var $mdgriffith$elm_ui$Internal$Model$FullTransform = F4(
	function (a, b, c, d) {
		return {$: 2, a: a, b: b, c: c, d: d};
	});
var $mdgriffith$elm_ui$Internal$Model$Moved = function (a) {
	return {$: 1, a: a};
};
var $mdgriffith$elm_ui$Internal$Model$composeTransformation = F2(
	function (transform, component) {
		switch (transform.$) {
			case 0:
				switch (component.$) {
					case 0:
						var x = component.a;
						return $mdgriffith$elm_ui$Internal$Model$Moved(
							_Utils_Tuple3(x, 0, 0));
					case 1:
						var y = component.a;
						return $mdgriffith$elm_ui$Internal$Model$Moved(
							_Utils_Tuple3(0, y, 0));
					case 2:
						var z = component.a;
						return $mdgriffith$elm_ui$Internal$Model$Moved(
							_Utils_Tuple3(0, 0, z));
					case 3:
						var xyz = component.a;
						return $mdgriffith$elm_ui$Internal$Model$Moved(xyz);
					case 4:
						var xyz = component.a;
						var angle = component.b;
						return A4(
							$mdgriffith$elm_ui$Internal$Model$FullTransform,
							_Utils_Tuple3(0, 0, 0),
							_Utils_Tuple3(1, 1, 1),
							xyz,
							angle);
					default:
						var xyz = component.a;
						return A4(
							$mdgriffith$elm_ui$Internal$Model$FullTransform,
							_Utils_Tuple3(0, 0, 0),
							xyz,
							_Utils_Tuple3(0, 0, 1),
							0);
				}
			case 1:
				var moved = transform.a;
				var x = moved.a;
				var y = moved.b;
				var z = moved.c;
				switch (component.$) {
					case 0:
						var newX = component.a;
						return $mdgriffith$elm_ui$Internal$Model$Moved(
							_Utils_Tuple3(newX, y, z));
					case 1:
						var newY = component.a;
						return $mdgriffith$elm_ui$Internal$Model$Moved(
							_Utils_Tuple3(x, newY, z));
					case 2:
						var newZ = component.a;
						return $mdgriffith$elm_ui$Internal$Model$Moved(
							_Utils_Tuple3(x, y, newZ));
					case 3:
						var xyz = component.a;
						return $mdgriffith$elm_ui$Internal$Model$Moved(xyz);
					case 4:
						var xyz = component.a;
						var angle = component.b;
						return A4(
							$mdgriffith$elm_ui$Internal$Model$FullTransform,
							moved,
							_Utils_Tuple3(1, 1, 1),
							xyz,
							angle);
					default:
						var scale = component.a;
						return A4(
							$mdgriffith$elm_ui$Internal$Model$FullTransform,
							moved,
							scale,
							_Utils_Tuple3(0, 0, 1),
							0);
				}
			default:
				var moved = transform.a;
				var x = moved.a;
				var y = moved.b;
				var z = moved.c;
				var scaled = transform.b;
				var origin = transform.c;
				var angle = transform.d;
				switch (component.$) {
					case 0:
						var newX = component.a;
						return A4(
							$mdgriffith$elm_ui$Internal$Model$FullTransform,
							_Utils_Tuple3(newX, y, z),
							scaled,
							origin,
							angle);
					case 1:
						var newY = component.a;
						return A4(
							$mdgriffith$elm_ui$Internal$Model$FullTransform,
							_Utils_Tuple3(x, newY, z),
							scaled,
							origin,
							angle);
					case 2:
						var newZ = component.a;
						return A4(
							$mdgriffith$elm_ui$Internal$Model$FullTransform,
							_Utils_Tuple3(x, y, newZ),
							scaled,
							origin,
							angle);
					case 3:
						var newMove = component.a;
						return A4($mdgriffith$elm_ui$Internal$Model$FullTransform, newMove, scaled, origin, angle);
					case 4:
						var newOrigin = component.a;
						var newAngle = component.b;
						return A4($mdgriffith$elm_ui$Internal$Model$FullTransform, moved, scaled, newOrigin, newAngle);
					default:
						var newScale = component.a;
						return A4($mdgriffith$elm_ui$Internal$Model$FullTransform, moved, newScale, origin, angle);
				}
		}
	});
var $mdgriffith$elm_ui$Internal$Flag$height = $mdgriffith$elm_ui$Internal$Flag$flag(7);
var $mdgriffith$elm_ui$Internal$Flag$heightContent = $mdgriffith$elm_ui$Internal$Flag$flag(36);
var $mdgriffith$elm_ui$Internal$Flag$merge = F2(
	function (_v0, _v1) {
		var one = _v0.a;
		var two = _v0.b;
		var three = _v1.a;
		var four = _v1.b;
		return A2($mdgriffith$elm_ui$Internal$Flag$Field, one | three, two | four);
	});
var $mdgriffith$elm_ui$Internal$Flag$none = A2($mdgriffith$elm_ui$Internal$Flag$Field, 0, 0);
var $mdgriffith$elm_ui$Internal$Model$renderHeight = function (h) {
	switch (h.$) {
		case 0:
			var px = h.a;
			var val = $elm$core$String$fromInt(px);
			var name = 'height-px-' + val;
			return _Utils_Tuple3(
				$mdgriffith$elm_ui$Internal$Flag$none,
				$mdgriffith$elm_ui$Internal$Style$classes.a4 + (' ' + name),
				_List_fromArray(
					[
						A3($mdgriffith$elm_ui$Internal$Model$Single, name, 'height', val + 'px')
					]));
		case 1:
			return _Utils_Tuple3(
				A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$heightContent, $mdgriffith$elm_ui$Internal$Flag$none),
				$mdgriffith$elm_ui$Internal$Style$classes.aH,
				_List_Nil);
		case 2:
			var portion = h.a;
			return (portion === 1) ? _Utils_Tuple3(
				A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$heightFill, $mdgriffith$elm_ui$Internal$Flag$none),
				$mdgriffith$elm_ui$Internal$Style$classes.aI,
				_List_Nil) : _Utils_Tuple3(
				A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$heightFill, $mdgriffith$elm_ui$Internal$Flag$none),
				$mdgriffith$elm_ui$Internal$Style$classes.a5 + (' height-fill-' + $elm$core$String$fromInt(portion)),
				_List_fromArray(
					[
						A3(
						$mdgriffith$elm_ui$Internal$Model$Single,
						$mdgriffith$elm_ui$Internal$Style$classes.bS + ('.' + ($mdgriffith$elm_ui$Internal$Style$classes.B + (' > ' + $mdgriffith$elm_ui$Internal$Style$dot(
							'height-fill-' + $elm$core$String$fromInt(portion))))),
						'flex-grow',
						$elm$core$String$fromInt(portion * 100000))
					]));
		case 3:
			var minSize = h.a;
			var len = h.b;
			var cls = 'min-height-' + $elm$core$String$fromInt(minSize);
			var style = A3(
				$mdgriffith$elm_ui$Internal$Model$Single,
				cls,
				'min-height',
				$elm$core$String$fromInt(minSize) + 'px');
			var _v1 = $mdgriffith$elm_ui$Internal$Model$renderHeight(len);
			var newFlag = _v1.a;
			var newAttrs = _v1.b;
			var newStyle = _v1.c;
			return _Utils_Tuple3(
				A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$heightBetween, newFlag),
				cls + (' ' + newAttrs),
				A2($elm$core$List$cons, style, newStyle));
		default:
			var maxSize = h.a;
			var len = h.b;
			var cls = 'max-height-' + $elm$core$String$fromInt(maxSize);
			var style = A3(
				$mdgriffith$elm_ui$Internal$Model$Single,
				cls,
				'max-height',
				$elm$core$String$fromInt(maxSize) + 'px');
			var _v2 = $mdgriffith$elm_ui$Internal$Model$renderHeight(len);
			var newFlag = _v2.a;
			var newAttrs = _v2.b;
			var newStyle = _v2.c;
			return _Utils_Tuple3(
				A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$heightBetween, newFlag),
				cls + (' ' + newAttrs),
				A2($elm$core$List$cons, style, newStyle));
	}
};
var $mdgriffith$elm_ui$Internal$Flag$widthContent = $mdgriffith$elm_ui$Internal$Flag$flag(38);
var $mdgriffith$elm_ui$Internal$Model$renderWidth = function (w) {
	switch (w.$) {
		case 0:
			var px = w.a;
			return _Utils_Tuple3(
				$mdgriffith$elm_ui$Internal$Flag$none,
				$mdgriffith$elm_ui$Internal$Style$classes.bC + (' width-px-' + $elm$core$String$fromInt(px)),
				_List_fromArray(
					[
						A3(
						$mdgriffith$elm_ui$Internal$Model$Single,
						'width-px-' + $elm$core$String$fromInt(px),
						'width',
						$elm$core$String$fromInt(px) + 'px')
					]));
		case 1:
			return _Utils_Tuple3(
				A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$widthContent, $mdgriffith$elm_ui$Internal$Flag$none),
				$mdgriffith$elm_ui$Internal$Style$classes.aS,
				_List_Nil);
		case 2:
			var portion = w.a;
			return (portion === 1) ? _Utils_Tuple3(
				A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$widthFill, $mdgriffith$elm_ui$Internal$Flag$none),
				$mdgriffith$elm_ui$Internal$Style$classes.aT,
				_List_Nil) : _Utils_Tuple3(
				A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$widthFill, $mdgriffith$elm_ui$Internal$Flag$none),
				$mdgriffith$elm_ui$Internal$Style$classes.bD + (' width-fill-' + $elm$core$String$fromInt(portion)),
				_List_fromArray(
					[
						A3(
						$mdgriffith$elm_ui$Internal$Model$Single,
						$mdgriffith$elm_ui$Internal$Style$classes.bS + ('.' + ($mdgriffith$elm_ui$Internal$Style$classes.s + (' > ' + $mdgriffith$elm_ui$Internal$Style$dot(
							'width-fill-' + $elm$core$String$fromInt(portion))))),
						'flex-grow',
						$elm$core$String$fromInt(portion * 100000))
					]));
		case 3:
			var minSize = w.a;
			var len = w.b;
			var cls = 'min-width-' + $elm$core$String$fromInt(minSize);
			var style = A3(
				$mdgriffith$elm_ui$Internal$Model$Single,
				cls,
				'min-width',
				$elm$core$String$fromInt(minSize) + 'px');
			var _v1 = $mdgriffith$elm_ui$Internal$Model$renderWidth(len);
			var newFlag = _v1.a;
			var newAttrs = _v1.b;
			var newStyle = _v1.c;
			return _Utils_Tuple3(
				A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$widthBetween, newFlag),
				cls + (' ' + newAttrs),
				A2($elm$core$List$cons, style, newStyle));
		default:
			var maxSize = w.a;
			var len = w.b;
			var cls = 'max-width-' + $elm$core$String$fromInt(maxSize);
			var style = A3(
				$mdgriffith$elm_ui$Internal$Model$Single,
				cls,
				'max-width',
				$elm$core$String$fromInt(maxSize) + 'px');
			var _v2 = $mdgriffith$elm_ui$Internal$Model$renderWidth(len);
			var newFlag = _v2.a;
			var newAttrs = _v2.b;
			var newStyle = _v2.c;
			return _Utils_Tuple3(
				A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$widthBetween, newFlag),
				cls + (' ' + newAttrs),
				A2($elm$core$List$cons, style, newStyle));
	}
};
var $mdgriffith$elm_ui$Internal$Flag$borderWidth = $mdgriffith$elm_ui$Internal$Flag$flag(27);
var $mdgriffith$elm_ui$Internal$Model$skippable = F2(
	function (flag, style) {
		if (_Utils_eq(flag, $mdgriffith$elm_ui$Internal$Flag$borderWidth)) {
			if (style.$ === 3) {
				var val = style.c;
				switch (val) {
					case '0px':
						return true;
					case '1px':
						return true;
					case '2px':
						return true;
					case '3px':
						return true;
					case '4px':
						return true;
					case '5px':
						return true;
					case '6px':
						return true;
					default:
						return false;
				}
			} else {
				return false;
			}
		} else {
			switch (style.$) {
				case 2:
					var i = style.a;
					return (i >= 8) && (i <= 32);
				case 7:
					var name = style.a;
					var t = style.b;
					var r = style.c;
					var b = style.d;
					var l = style.e;
					return _Utils_eq(t, b) && (_Utils_eq(t, r) && (_Utils_eq(t, l) && ((t >= 0) && (t <= 24))));
				default:
					return false;
			}
		}
	});
var $mdgriffith$elm_ui$Internal$Flag$width = $mdgriffith$elm_ui$Internal$Flag$flag(6);
var $mdgriffith$elm_ui$Internal$Flag$xAlign = $mdgriffith$elm_ui$Internal$Flag$flag(30);
var $mdgriffith$elm_ui$Internal$Flag$yAlign = $mdgriffith$elm_ui$Internal$Flag$flag(29);
var $mdgriffith$elm_ui$Internal$Model$gatherAttrRecursive = F8(
	function (classes, node, has, transform, styles, attrs, children, elementAttrs) {
		gatherAttrRecursive:
		while (true) {
			if (!elementAttrs.b) {
				var _v1 = $mdgriffith$elm_ui$Internal$Model$transformClass(transform);
				if (_v1.$ === 1) {
					return {
						G: A2(
							$elm$core$List$cons,
							$elm$html$Html$Attributes$class(classes),
							attrs),
						H: children,
						K: has,
						L: node,
						c4: styles
					};
				} else {
					var _class = _v1.a;
					return {
						G: A2(
							$elm$core$List$cons,
							$elm$html$Html$Attributes$class(classes + (' ' + _class)),
							attrs),
						H: children,
						K: has,
						L: node,
						c4: A2(
							$elm$core$List$cons,
							$mdgriffith$elm_ui$Internal$Model$Transform(transform),
							styles)
					};
				}
			} else {
				var attribute = elementAttrs.a;
				var remaining = elementAttrs.b;
				switch (attribute.$) {
					case 0:
						var $temp$classes = classes,
							$temp$node = node,
							$temp$has = has,
							$temp$transform = transform,
							$temp$styles = styles,
							$temp$attrs = attrs,
							$temp$children = children,
							$temp$elementAttrs = remaining;
						classes = $temp$classes;
						node = $temp$node;
						has = $temp$has;
						transform = $temp$transform;
						styles = $temp$styles;
						attrs = $temp$attrs;
						children = $temp$children;
						elementAttrs = $temp$elementAttrs;
						continue gatherAttrRecursive;
					case 3:
						var flag = attribute.a;
						var exactClassName = attribute.b;
						if (A2($mdgriffith$elm_ui$Internal$Flag$present, flag, has)) {
							var $temp$classes = classes,
								$temp$node = node,
								$temp$has = has,
								$temp$transform = transform,
								$temp$styles = styles,
								$temp$attrs = attrs,
								$temp$children = children,
								$temp$elementAttrs = remaining;
							classes = $temp$classes;
							node = $temp$node;
							has = $temp$has;
							transform = $temp$transform;
							styles = $temp$styles;
							attrs = $temp$attrs;
							children = $temp$children;
							elementAttrs = $temp$elementAttrs;
							continue gatherAttrRecursive;
						} else {
							var $temp$classes = exactClassName + (' ' + classes),
								$temp$node = node,
								$temp$has = A2($mdgriffith$elm_ui$Internal$Flag$add, flag, has),
								$temp$transform = transform,
								$temp$styles = styles,
								$temp$attrs = attrs,
								$temp$children = children,
								$temp$elementAttrs = remaining;
							classes = $temp$classes;
							node = $temp$node;
							has = $temp$has;
							transform = $temp$transform;
							styles = $temp$styles;
							attrs = $temp$attrs;
							children = $temp$children;
							elementAttrs = $temp$elementAttrs;
							continue gatherAttrRecursive;
						}
					case 1:
						var actualAttribute = attribute.a;
						var $temp$classes = classes,
							$temp$node = node,
							$temp$has = has,
							$temp$transform = transform,
							$temp$styles = styles,
							$temp$attrs = A2($elm$core$List$cons, actualAttribute, attrs),
							$temp$children = children,
							$temp$elementAttrs = remaining;
						classes = $temp$classes;
						node = $temp$node;
						has = $temp$has;
						transform = $temp$transform;
						styles = $temp$styles;
						attrs = $temp$attrs;
						children = $temp$children;
						elementAttrs = $temp$elementAttrs;
						continue gatherAttrRecursive;
					case 4:
						var flag = attribute.a;
						var style = attribute.b;
						if (A2($mdgriffith$elm_ui$Internal$Flag$present, flag, has)) {
							var $temp$classes = classes,
								$temp$node = node,
								$temp$has = has,
								$temp$transform = transform,
								$temp$styles = styles,
								$temp$attrs = attrs,
								$temp$children = children,
								$temp$elementAttrs = remaining;
							classes = $temp$classes;
							node = $temp$node;
							has = $temp$has;
							transform = $temp$transform;
							styles = $temp$styles;
							attrs = $temp$attrs;
							children = $temp$children;
							elementAttrs = $temp$elementAttrs;
							continue gatherAttrRecursive;
						} else {
							if (A2($mdgriffith$elm_ui$Internal$Model$skippable, flag, style)) {
								var $temp$classes = $mdgriffith$elm_ui$Internal$Model$getStyleName(style) + (' ' + classes),
									$temp$node = node,
									$temp$has = A2($mdgriffith$elm_ui$Internal$Flag$add, flag, has),
									$temp$transform = transform,
									$temp$styles = styles,
									$temp$attrs = attrs,
									$temp$children = children,
									$temp$elementAttrs = remaining;
								classes = $temp$classes;
								node = $temp$node;
								has = $temp$has;
								transform = $temp$transform;
								styles = $temp$styles;
								attrs = $temp$attrs;
								children = $temp$children;
								elementAttrs = $temp$elementAttrs;
								continue gatherAttrRecursive;
							} else {
								var $temp$classes = $mdgriffith$elm_ui$Internal$Model$getStyleName(style) + (' ' + classes),
									$temp$node = node,
									$temp$has = A2($mdgriffith$elm_ui$Internal$Flag$add, flag, has),
									$temp$transform = transform,
									$temp$styles = A2($elm$core$List$cons, style, styles),
									$temp$attrs = attrs,
									$temp$children = children,
									$temp$elementAttrs = remaining;
								classes = $temp$classes;
								node = $temp$node;
								has = $temp$has;
								transform = $temp$transform;
								styles = $temp$styles;
								attrs = $temp$attrs;
								children = $temp$children;
								elementAttrs = $temp$elementAttrs;
								continue gatherAttrRecursive;
							}
						}
					case 10:
						var flag = attribute.a;
						var component = attribute.b;
						var $temp$classes = classes,
							$temp$node = node,
							$temp$has = A2($mdgriffith$elm_ui$Internal$Flag$add, flag, has),
							$temp$transform = A2($mdgriffith$elm_ui$Internal$Model$composeTransformation, transform, component),
							$temp$styles = styles,
							$temp$attrs = attrs,
							$temp$children = children,
							$temp$elementAttrs = remaining;
						classes = $temp$classes;
						node = $temp$node;
						has = $temp$has;
						transform = $temp$transform;
						styles = $temp$styles;
						attrs = $temp$attrs;
						children = $temp$children;
						elementAttrs = $temp$elementAttrs;
						continue gatherAttrRecursive;
					case 7:
						var width = attribute.a;
						if (A2($mdgriffith$elm_ui$Internal$Flag$present, $mdgriffith$elm_ui$Internal$Flag$width, has)) {
							var $temp$classes = classes,
								$temp$node = node,
								$temp$has = has,
								$temp$transform = transform,
								$temp$styles = styles,
								$temp$attrs = attrs,
								$temp$children = children,
								$temp$elementAttrs = remaining;
							classes = $temp$classes;
							node = $temp$node;
							has = $temp$has;
							transform = $temp$transform;
							styles = $temp$styles;
							attrs = $temp$attrs;
							children = $temp$children;
							elementAttrs = $temp$elementAttrs;
							continue gatherAttrRecursive;
						} else {
							switch (width.$) {
								case 0:
									var px = width.a;
									var $temp$classes = ($mdgriffith$elm_ui$Internal$Style$classes.bC + (' width-px-' + $elm$core$String$fromInt(px))) + (' ' + classes),
										$temp$node = node,
										$temp$has = A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$width, has),
										$temp$transform = transform,
										$temp$styles = A2(
										$elm$core$List$cons,
										A3(
											$mdgriffith$elm_ui$Internal$Model$Single,
											'width-px-' + $elm$core$String$fromInt(px),
											'width',
											$elm$core$String$fromInt(px) + 'px'),
										styles),
										$temp$attrs = attrs,
										$temp$children = children,
										$temp$elementAttrs = remaining;
									classes = $temp$classes;
									node = $temp$node;
									has = $temp$has;
									transform = $temp$transform;
									styles = $temp$styles;
									attrs = $temp$attrs;
									children = $temp$children;
									elementAttrs = $temp$elementAttrs;
									continue gatherAttrRecursive;
								case 1:
									var $temp$classes = classes + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.aS),
										$temp$node = node,
										$temp$has = A2(
										$mdgriffith$elm_ui$Internal$Flag$add,
										$mdgriffith$elm_ui$Internal$Flag$widthContent,
										A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$width, has)),
										$temp$transform = transform,
										$temp$styles = styles,
										$temp$attrs = attrs,
										$temp$children = children,
										$temp$elementAttrs = remaining;
									classes = $temp$classes;
									node = $temp$node;
									has = $temp$has;
									transform = $temp$transform;
									styles = $temp$styles;
									attrs = $temp$attrs;
									children = $temp$children;
									elementAttrs = $temp$elementAttrs;
									continue gatherAttrRecursive;
								case 2:
									var portion = width.a;
									if (portion === 1) {
										var $temp$classes = classes + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.aT),
											$temp$node = node,
											$temp$has = A2(
											$mdgriffith$elm_ui$Internal$Flag$add,
											$mdgriffith$elm_ui$Internal$Flag$widthFill,
											A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$width, has)),
											$temp$transform = transform,
											$temp$styles = styles,
											$temp$attrs = attrs,
											$temp$children = children,
											$temp$elementAttrs = remaining;
										classes = $temp$classes;
										node = $temp$node;
										has = $temp$has;
										transform = $temp$transform;
										styles = $temp$styles;
										attrs = $temp$attrs;
										children = $temp$children;
										elementAttrs = $temp$elementAttrs;
										continue gatherAttrRecursive;
									} else {
										var $temp$classes = classes + (' ' + ($mdgriffith$elm_ui$Internal$Style$classes.bD + (' width-fill-' + $elm$core$String$fromInt(portion)))),
											$temp$node = node,
											$temp$has = A2(
											$mdgriffith$elm_ui$Internal$Flag$add,
											$mdgriffith$elm_ui$Internal$Flag$widthFill,
											A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$width, has)),
											$temp$transform = transform,
											$temp$styles = A2(
											$elm$core$List$cons,
											A3(
												$mdgriffith$elm_ui$Internal$Model$Single,
												$mdgriffith$elm_ui$Internal$Style$classes.bS + ('.' + ($mdgriffith$elm_ui$Internal$Style$classes.s + (' > ' + $mdgriffith$elm_ui$Internal$Style$dot(
													'width-fill-' + $elm$core$String$fromInt(portion))))),
												'flex-grow',
												$elm$core$String$fromInt(portion * 100000)),
											styles),
											$temp$attrs = attrs,
											$temp$children = children,
											$temp$elementAttrs = remaining;
										classes = $temp$classes;
										node = $temp$node;
										has = $temp$has;
										transform = $temp$transform;
										styles = $temp$styles;
										attrs = $temp$attrs;
										children = $temp$children;
										elementAttrs = $temp$elementAttrs;
										continue gatherAttrRecursive;
									}
								default:
									var _v4 = $mdgriffith$elm_ui$Internal$Model$renderWidth(width);
									var addToFlags = _v4.a;
									var newClass = _v4.b;
									var newStyles = _v4.c;
									var $temp$classes = classes + (' ' + newClass),
										$temp$node = node,
										$temp$has = A2(
										$mdgriffith$elm_ui$Internal$Flag$merge,
										addToFlags,
										A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$width, has)),
										$temp$transform = transform,
										$temp$styles = _Utils_ap(newStyles, styles),
										$temp$attrs = attrs,
										$temp$children = children,
										$temp$elementAttrs = remaining;
									classes = $temp$classes;
									node = $temp$node;
									has = $temp$has;
									transform = $temp$transform;
									styles = $temp$styles;
									attrs = $temp$attrs;
									children = $temp$children;
									elementAttrs = $temp$elementAttrs;
									continue gatherAttrRecursive;
							}
						}
					case 8:
						var height = attribute.a;
						if (A2($mdgriffith$elm_ui$Internal$Flag$present, $mdgriffith$elm_ui$Internal$Flag$height, has)) {
							var $temp$classes = classes,
								$temp$node = node,
								$temp$has = has,
								$temp$transform = transform,
								$temp$styles = styles,
								$temp$attrs = attrs,
								$temp$children = children,
								$temp$elementAttrs = remaining;
							classes = $temp$classes;
							node = $temp$node;
							has = $temp$has;
							transform = $temp$transform;
							styles = $temp$styles;
							attrs = $temp$attrs;
							children = $temp$children;
							elementAttrs = $temp$elementAttrs;
							continue gatherAttrRecursive;
						} else {
							switch (height.$) {
								case 0:
									var px = height.a;
									var val = $elm$core$String$fromInt(px) + 'px';
									var name = 'height-px-' + val;
									var $temp$classes = $mdgriffith$elm_ui$Internal$Style$classes.a4 + (' ' + (name + (' ' + classes))),
										$temp$node = node,
										$temp$has = A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$height, has),
										$temp$transform = transform,
										$temp$styles = A2(
										$elm$core$List$cons,
										A3($mdgriffith$elm_ui$Internal$Model$Single, name, 'height ', val),
										styles),
										$temp$attrs = attrs,
										$temp$children = children,
										$temp$elementAttrs = remaining;
									classes = $temp$classes;
									node = $temp$node;
									has = $temp$has;
									transform = $temp$transform;
									styles = $temp$styles;
									attrs = $temp$attrs;
									children = $temp$children;
									elementAttrs = $temp$elementAttrs;
									continue gatherAttrRecursive;
								case 1:
									var $temp$classes = $mdgriffith$elm_ui$Internal$Style$classes.aH + (' ' + classes),
										$temp$node = node,
										$temp$has = A2(
										$mdgriffith$elm_ui$Internal$Flag$add,
										$mdgriffith$elm_ui$Internal$Flag$heightContent,
										A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$height, has)),
										$temp$transform = transform,
										$temp$styles = styles,
										$temp$attrs = attrs,
										$temp$children = children,
										$temp$elementAttrs = remaining;
									classes = $temp$classes;
									node = $temp$node;
									has = $temp$has;
									transform = $temp$transform;
									styles = $temp$styles;
									attrs = $temp$attrs;
									children = $temp$children;
									elementAttrs = $temp$elementAttrs;
									continue gatherAttrRecursive;
								case 2:
									var portion = height.a;
									if (portion === 1) {
										var $temp$classes = $mdgriffith$elm_ui$Internal$Style$classes.aI + (' ' + classes),
											$temp$node = node,
											$temp$has = A2(
											$mdgriffith$elm_ui$Internal$Flag$add,
											$mdgriffith$elm_ui$Internal$Flag$heightFill,
											A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$height, has)),
											$temp$transform = transform,
											$temp$styles = styles,
											$temp$attrs = attrs,
											$temp$children = children,
											$temp$elementAttrs = remaining;
										classes = $temp$classes;
										node = $temp$node;
										has = $temp$has;
										transform = $temp$transform;
										styles = $temp$styles;
										attrs = $temp$attrs;
										children = $temp$children;
										elementAttrs = $temp$elementAttrs;
										continue gatherAttrRecursive;
									} else {
										var $temp$classes = classes + (' ' + ($mdgriffith$elm_ui$Internal$Style$classes.a5 + (' height-fill-' + $elm$core$String$fromInt(portion)))),
											$temp$node = node,
											$temp$has = A2(
											$mdgriffith$elm_ui$Internal$Flag$add,
											$mdgriffith$elm_ui$Internal$Flag$heightFill,
											A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$height, has)),
											$temp$transform = transform,
											$temp$styles = A2(
											$elm$core$List$cons,
											A3(
												$mdgriffith$elm_ui$Internal$Model$Single,
												$mdgriffith$elm_ui$Internal$Style$classes.bS + ('.' + ($mdgriffith$elm_ui$Internal$Style$classes.B + (' > ' + $mdgriffith$elm_ui$Internal$Style$dot(
													'height-fill-' + $elm$core$String$fromInt(portion))))),
												'flex-grow',
												$elm$core$String$fromInt(portion * 100000)),
											styles),
											$temp$attrs = attrs,
											$temp$children = children,
											$temp$elementAttrs = remaining;
										classes = $temp$classes;
										node = $temp$node;
										has = $temp$has;
										transform = $temp$transform;
										styles = $temp$styles;
										attrs = $temp$attrs;
										children = $temp$children;
										elementAttrs = $temp$elementAttrs;
										continue gatherAttrRecursive;
									}
								default:
									var _v6 = $mdgriffith$elm_ui$Internal$Model$renderHeight(height);
									var addToFlags = _v6.a;
									var newClass = _v6.b;
									var newStyles = _v6.c;
									var $temp$classes = classes + (' ' + newClass),
										$temp$node = node,
										$temp$has = A2(
										$mdgriffith$elm_ui$Internal$Flag$merge,
										addToFlags,
										A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$height, has)),
										$temp$transform = transform,
										$temp$styles = _Utils_ap(newStyles, styles),
										$temp$attrs = attrs,
										$temp$children = children,
										$temp$elementAttrs = remaining;
									classes = $temp$classes;
									node = $temp$node;
									has = $temp$has;
									transform = $temp$transform;
									styles = $temp$styles;
									attrs = $temp$attrs;
									children = $temp$children;
									elementAttrs = $temp$elementAttrs;
									continue gatherAttrRecursive;
							}
						}
					case 2:
						var description = attribute.a;
						switch (description.$) {
							case 0:
								var $temp$classes = classes,
									$temp$node = A2($mdgriffith$elm_ui$Internal$Model$addNodeName, 'main', node),
									$temp$has = has,
									$temp$transform = transform,
									$temp$styles = styles,
									$temp$attrs = attrs,
									$temp$children = children,
									$temp$elementAttrs = remaining;
								classes = $temp$classes;
								node = $temp$node;
								has = $temp$has;
								transform = $temp$transform;
								styles = $temp$styles;
								attrs = $temp$attrs;
								children = $temp$children;
								elementAttrs = $temp$elementAttrs;
								continue gatherAttrRecursive;
							case 1:
								var $temp$classes = classes,
									$temp$node = A2($mdgriffith$elm_ui$Internal$Model$addNodeName, 'nav', node),
									$temp$has = has,
									$temp$transform = transform,
									$temp$styles = styles,
									$temp$attrs = attrs,
									$temp$children = children,
									$temp$elementAttrs = remaining;
								classes = $temp$classes;
								node = $temp$node;
								has = $temp$has;
								transform = $temp$transform;
								styles = $temp$styles;
								attrs = $temp$attrs;
								children = $temp$children;
								elementAttrs = $temp$elementAttrs;
								continue gatherAttrRecursive;
							case 2:
								var $temp$classes = classes,
									$temp$node = A2($mdgriffith$elm_ui$Internal$Model$addNodeName, 'footer', node),
									$temp$has = has,
									$temp$transform = transform,
									$temp$styles = styles,
									$temp$attrs = attrs,
									$temp$children = children,
									$temp$elementAttrs = remaining;
								classes = $temp$classes;
								node = $temp$node;
								has = $temp$has;
								transform = $temp$transform;
								styles = $temp$styles;
								attrs = $temp$attrs;
								children = $temp$children;
								elementAttrs = $temp$elementAttrs;
								continue gatherAttrRecursive;
							case 3:
								var $temp$classes = classes,
									$temp$node = A2($mdgriffith$elm_ui$Internal$Model$addNodeName, 'aside', node),
									$temp$has = has,
									$temp$transform = transform,
									$temp$styles = styles,
									$temp$attrs = attrs,
									$temp$children = children,
									$temp$elementAttrs = remaining;
								classes = $temp$classes;
								node = $temp$node;
								has = $temp$has;
								transform = $temp$transform;
								styles = $temp$styles;
								attrs = $temp$attrs;
								children = $temp$children;
								elementAttrs = $temp$elementAttrs;
								continue gatherAttrRecursive;
							case 4:
								var i = description.a;
								if (i <= 1) {
									var $temp$classes = classes,
										$temp$node = A2($mdgriffith$elm_ui$Internal$Model$addNodeName, 'h1', node),
										$temp$has = has,
										$temp$transform = transform,
										$temp$styles = styles,
										$temp$attrs = attrs,
										$temp$children = children,
										$temp$elementAttrs = remaining;
									classes = $temp$classes;
									node = $temp$node;
									has = $temp$has;
									transform = $temp$transform;
									styles = $temp$styles;
									attrs = $temp$attrs;
									children = $temp$children;
									elementAttrs = $temp$elementAttrs;
									continue gatherAttrRecursive;
								} else {
									if (i < 7) {
										var $temp$classes = classes,
											$temp$node = A2(
											$mdgriffith$elm_ui$Internal$Model$addNodeName,
											'h' + $elm$core$String$fromInt(i),
											node),
											$temp$has = has,
											$temp$transform = transform,
											$temp$styles = styles,
											$temp$attrs = attrs,
											$temp$children = children,
											$temp$elementAttrs = remaining;
										classes = $temp$classes;
										node = $temp$node;
										has = $temp$has;
										transform = $temp$transform;
										styles = $temp$styles;
										attrs = $temp$attrs;
										children = $temp$children;
										elementAttrs = $temp$elementAttrs;
										continue gatherAttrRecursive;
									} else {
										var $temp$classes = classes,
											$temp$node = A2($mdgriffith$elm_ui$Internal$Model$addNodeName, 'h6', node),
											$temp$has = has,
											$temp$transform = transform,
											$temp$styles = styles,
											$temp$attrs = attrs,
											$temp$children = children,
											$temp$elementAttrs = remaining;
										classes = $temp$classes;
										node = $temp$node;
										has = $temp$has;
										transform = $temp$transform;
										styles = $temp$styles;
										attrs = $temp$attrs;
										children = $temp$children;
										elementAttrs = $temp$elementAttrs;
										continue gatherAttrRecursive;
									}
								}
							case 9:
								var $temp$classes = classes,
									$temp$node = node,
									$temp$has = has,
									$temp$transform = transform,
									$temp$styles = styles,
									$temp$attrs = attrs,
									$temp$children = children,
									$temp$elementAttrs = remaining;
								classes = $temp$classes;
								node = $temp$node;
								has = $temp$has;
								transform = $temp$transform;
								styles = $temp$styles;
								attrs = $temp$attrs;
								children = $temp$children;
								elementAttrs = $temp$elementAttrs;
								continue gatherAttrRecursive;
							case 8:
								var $temp$classes = classes,
									$temp$node = node,
									$temp$has = has,
									$temp$transform = transform,
									$temp$styles = styles,
									$temp$attrs = A2(
									$elm$core$List$cons,
									A2($elm$virtual_dom$VirtualDom$attribute, 'role', 'button'),
									attrs),
									$temp$children = children,
									$temp$elementAttrs = remaining;
								classes = $temp$classes;
								node = $temp$node;
								has = $temp$has;
								transform = $temp$transform;
								styles = $temp$styles;
								attrs = $temp$attrs;
								children = $temp$children;
								elementAttrs = $temp$elementAttrs;
								continue gatherAttrRecursive;
							case 5:
								var label = description.a;
								var $temp$classes = classes,
									$temp$node = node,
									$temp$has = has,
									$temp$transform = transform,
									$temp$styles = styles,
									$temp$attrs = A2(
									$elm$core$List$cons,
									A2($elm$virtual_dom$VirtualDom$attribute, 'aria-label', label),
									attrs),
									$temp$children = children,
									$temp$elementAttrs = remaining;
								classes = $temp$classes;
								node = $temp$node;
								has = $temp$has;
								transform = $temp$transform;
								styles = $temp$styles;
								attrs = $temp$attrs;
								children = $temp$children;
								elementAttrs = $temp$elementAttrs;
								continue gatherAttrRecursive;
							case 6:
								var $temp$classes = classes,
									$temp$node = node,
									$temp$has = has,
									$temp$transform = transform,
									$temp$styles = styles,
									$temp$attrs = A2(
									$elm$core$List$cons,
									A2($elm$virtual_dom$VirtualDom$attribute, 'aria-live', 'polite'),
									attrs),
									$temp$children = children,
									$temp$elementAttrs = remaining;
								classes = $temp$classes;
								node = $temp$node;
								has = $temp$has;
								transform = $temp$transform;
								styles = $temp$styles;
								attrs = $temp$attrs;
								children = $temp$children;
								elementAttrs = $temp$elementAttrs;
								continue gatherAttrRecursive;
							default:
								var $temp$classes = classes,
									$temp$node = node,
									$temp$has = has,
									$temp$transform = transform,
									$temp$styles = styles,
									$temp$attrs = A2(
									$elm$core$List$cons,
									A2($elm$virtual_dom$VirtualDom$attribute, 'aria-live', 'assertive'),
									attrs),
									$temp$children = children,
									$temp$elementAttrs = remaining;
								classes = $temp$classes;
								node = $temp$node;
								has = $temp$has;
								transform = $temp$transform;
								styles = $temp$styles;
								attrs = $temp$attrs;
								children = $temp$children;
								elementAttrs = $temp$elementAttrs;
								continue gatherAttrRecursive;
						}
					case 9:
						var location = attribute.a;
						var elem = attribute.b;
						var newStyles = function () {
							switch (elem.$) {
								case 3:
									return styles;
								case 2:
									var str = elem.a;
									return styles;
								case 0:
									var html = elem.a;
									return styles;
								default:
									var styled = elem.a;
									return _Utils_ap(styles, styled.c4);
							}
						}();
						var $temp$classes = classes,
							$temp$node = node,
							$temp$has = has,
							$temp$transform = transform,
							$temp$styles = newStyles,
							$temp$attrs = attrs,
							$temp$children = A3($mdgriffith$elm_ui$Internal$Model$addNearbyElement, location, elem, children),
							$temp$elementAttrs = remaining;
						classes = $temp$classes;
						node = $temp$node;
						has = $temp$has;
						transform = $temp$transform;
						styles = $temp$styles;
						attrs = $temp$attrs;
						children = $temp$children;
						elementAttrs = $temp$elementAttrs;
						continue gatherAttrRecursive;
					case 6:
						var x = attribute.a;
						if (A2($mdgriffith$elm_ui$Internal$Flag$present, $mdgriffith$elm_ui$Internal$Flag$xAlign, has)) {
							var $temp$classes = classes,
								$temp$node = node,
								$temp$has = has,
								$temp$transform = transform,
								$temp$styles = styles,
								$temp$attrs = attrs,
								$temp$children = children,
								$temp$elementAttrs = remaining;
							classes = $temp$classes;
							node = $temp$node;
							has = $temp$has;
							transform = $temp$transform;
							styles = $temp$styles;
							attrs = $temp$attrs;
							children = $temp$children;
							elementAttrs = $temp$elementAttrs;
							continue gatherAttrRecursive;
						} else {
							var $temp$classes = $mdgriffith$elm_ui$Internal$Model$alignXName(x) + (' ' + classes),
								$temp$node = node,
								$temp$has = function (flags) {
								switch (x) {
									case 1:
										return A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$centerX, flags);
									case 2:
										return A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$alignRight, flags);
									default:
										return flags;
								}
							}(
								A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$xAlign, has)),
								$temp$transform = transform,
								$temp$styles = styles,
								$temp$attrs = attrs,
								$temp$children = children,
								$temp$elementAttrs = remaining;
							classes = $temp$classes;
							node = $temp$node;
							has = $temp$has;
							transform = $temp$transform;
							styles = $temp$styles;
							attrs = $temp$attrs;
							children = $temp$children;
							elementAttrs = $temp$elementAttrs;
							continue gatherAttrRecursive;
						}
					default:
						var y = attribute.a;
						if (A2($mdgriffith$elm_ui$Internal$Flag$present, $mdgriffith$elm_ui$Internal$Flag$yAlign, has)) {
							var $temp$classes = classes,
								$temp$node = node,
								$temp$has = has,
								$temp$transform = transform,
								$temp$styles = styles,
								$temp$attrs = attrs,
								$temp$children = children,
								$temp$elementAttrs = remaining;
							classes = $temp$classes;
							node = $temp$node;
							has = $temp$has;
							transform = $temp$transform;
							styles = $temp$styles;
							attrs = $temp$attrs;
							children = $temp$children;
							elementAttrs = $temp$elementAttrs;
							continue gatherAttrRecursive;
						} else {
							var $temp$classes = $mdgriffith$elm_ui$Internal$Model$alignYName(y) + (' ' + classes),
								$temp$node = node,
								$temp$has = function (flags) {
								switch (y) {
									case 1:
										return A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$centerY, flags);
									case 2:
										return A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$alignBottom, flags);
									default:
										return flags;
								}
							}(
								A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$yAlign, has)),
								$temp$transform = transform,
								$temp$styles = styles,
								$temp$attrs = attrs,
								$temp$children = children,
								$temp$elementAttrs = remaining;
							classes = $temp$classes;
							node = $temp$node;
							has = $temp$has;
							transform = $temp$transform;
							styles = $temp$styles;
							attrs = $temp$attrs;
							children = $temp$children;
							elementAttrs = $temp$elementAttrs;
							continue gatherAttrRecursive;
						}
				}
			}
		}
	});
var $mdgriffith$elm_ui$Internal$Model$Untransformed = {$: 0};
var $mdgriffith$elm_ui$Internal$Model$untransformed = $mdgriffith$elm_ui$Internal$Model$Untransformed;
var $mdgriffith$elm_ui$Internal$Model$element = F4(
	function (context, node, attributes, children) {
		return A3(
			$mdgriffith$elm_ui$Internal$Model$createElement,
			context,
			children,
			A8(
				$mdgriffith$elm_ui$Internal$Model$gatherAttrRecursive,
				$mdgriffith$elm_ui$Internal$Model$contextClasses(context),
				node,
				$mdgriffith$elm_ui$Internal$Flag$none,
				$mdgriffith$elm_ui$Internal$Model$untransformed,
				_List_Nil,
				_List_Nil,
				$mdgriffith$elm_ui$Internal$Model$NoNearbyChildren,
				$elm$core$List$reverse(attributes)));
	});
var $mdgriffith$elm_ui$Internal$Model$NoAttribute = {$: 0};
var $mdgriffith$elm_ui$Element$Input$hasFocusStyle = function (attr) {
	if (((attr.$ === 4) && (attr.b.$ === 11)) && (!attr.b.a)) {
		var _v1 = attr.b;
		var _v2 = _v1.a;
		return true;
	} else {
		return false;
	}
};
var $mdgriffith$elm_ui$Internal$Model$htmlClass = function (cls) {
	return $mdgriffith$elm_ui$Internal$Model$Attr(
		$elm$html$Html$Attributes$class(cls));
};
var $mdgriffith$elm_ui$Element$Input$focusDefault = function (attrs) {
	return A2($elm$core$List$any, $mdgriffith$elm_ui$Element$Input$hasFocusStyle, attrs) ? $mdgriffith$elm_ui$Internal$Model$NoAttribute : $mdgriffith$elm_ui$Internal$Model$htmlClass('focusable');
};
var $mdgriffith$elm_ui$Internal$Model$Height = function (a) {
	return {$: 8, a: a};
};
var $mdgriffith$elm_ui$Element$height = $mdgriffith$elm_ui$Internal$Model$Height;
var $elm$core$Basics$composeL = F3(
	function (g, f, x) {
		return g(
			f(x));
	});
var $elm$virtual_dom$VirtualDom$Normal = function (a) {
	return {$: 0, a: a};
};
var $elm$virtual_dom$VirtualDom$on = _VirtualDom_on;
var $elm$html$Html$Events$on = F2(
	function (event, decoder) {
		return A2(
			$elm$virtual_dom$VirtualDom$on,
			event,
			$elm$virtual_dom$VirtualDom$Normal(decoder));
	});
var $elm$html$Html$Events$onClick = function (msg) {
	return A2(
		$elm$html$Html$Events$on,
		'click',
		$elm$json$Json$Decode$succeed(msg));
};
var $mdgriffith$elm_ui$Element$Events$onClick = A2($elm$core$Basics$composeL, $mdgriffith$elm_ui$Internal$Model$Attr, $elm$html$Html$Events$onClick);
var $mdgriffith$elm_ui$Element$Input$enter = 'Enter';
var $elm$json$Json$Decode$andThen = _Json_andThen;
var $elm$json$Json$Decode$fail = _Json_fail;
var $elm$virtual_dom$VirtualDom$MayPreventDefault = function (a) {
	return {$: 2, a: a};
};
var $elm$html$Html$Events$preventDefaultOn = F2(
	function (event, decoder) {
		return A2(
			$elm$virtual_dom$VirtualDom$on,
			event,
			$elm$virtual_dom$VirtualDom$MayPreventDefault(decoder));
	});
var $elm$json$Json$Decode$string = _Json_decodeString;
var $mdgriffith$elm_ui$Element$Input$onKey = F2(
	function (desiredCode, msg) {
		var decode = function (code) {
			return _Utils_eq(code, desiredCode) ? $elm$json$Json$Decode$succeed(msg) : $elm$json$Json$Decode$fail('Not the enter key');
		};
		var isKey = A2(
			$elm$json$Json$Decode$andThen,
			decode,
			A2($elm$json$Json$Decode$field, 'key', $elm$json$Json$Decode$string));
		return $mdgriffith$elm_ui$Internal$Model$Attr(
			A2(
				$elm$html$Html$Events$preventDefaultOn,
				'keyup',
				A2(
					$elm$json$Json$Decode$map,
					function (fired) {
						return _Utils_Tuple2(fired, true);
					},
					isKey)));
	});
var $mdgriffith$elm_ui$Element$Input$onEnter = function (msg) {
	return A2($mdgriffith$elm_ui$Element$Input$onKey, $mdgriffith$elm_ui$Element$Input$enter, msg);
};
var $mdgriffith$elm_ui$Internal$Model$Class = F2(
	function (a, b) {
		return {$: 3, a: a, b: b};
	});
var $mdgriffith$elm_ui$Internal$Flag$cursor = $mdgriffith$elm_ui$Internal$Flag$flag(21);
var $mdgriffith$elm_ui$Element$pointer = A2($mdgriffith$elm_ui$Internal$Model$Class, $mdgriffith$elm_ui$Internal$Flag$cursor, $mdgriffith$elm_ui$Internal$Style$classes.cd);
var $mdgriffith$elm_ui$Internal$Model$Content = {$: 1};
var $mdgriffith$elm_ui$Element$shrink = $mdgriffith$elm_ui$Internal$Model$Content;
var $elm$html$Html$Attributes$tabindex = function (n) {
	return A2(
		_VirtualDom_attribute,
		'tabIndex',
		$elm$core$String$fromInt(n));
};
var $mdgriffith$elm_ui$Internal$Model$Width = function (a) {
	return {$: 7, a: a};
};
var $mdgriffith$elm_ui$Element$width = $mdgriffith$elm_ui$Internal$Model$Width;
var $mdgriffith$elm_ui$Element$Input$button = F2(
	function (attrs, _v0) {
		var onPress = _v0.cG;
		var label = _v0.ba;
		return A4(
			$mdgriffith$elm_ui$Internal$Model$element,
			$mdgriffith$elm_ui$Internal$Model$asEl,
			$mdgriffith$elm_ui$Internal$Model$div,
			A2(
				$elm$core$List$cons,
				$mdgriffith$elm_ui$Element$width($mdgriffith$elm_ui$Element$shrink),
				A2(
					$elm$core$List$cons,
					$mdgriffith$elm_ui$Element$height($mdgriffith$elm_ui$Element$shrink),
					A2(
						$elm$core$List$cons,
						$mdgriffith$elm_ui$Internal$Model$htmlClass($mdgriffith$elm_ui$Internal$Style$classes.ar + (' ' + ($mdgriffith$elm_ui$Internal$Style$classes.C + (' ' + ($mdgriffith$elm_ui$Internal$Style$classes.cV + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.bf)))))),
						A2(
							$elm$core$List$cons,
							$mdgriffith$elm_ui$Element$pointer,
							A2(
								$elm$core$List$cons,
								$mdgriffith$elm_ui$Element$Input$focusDefault(attrs),
								A2(
									$elm$core$List$cons,
									$mdgriffith$elm_ui$Internal$Model$Describe($mdgriffith$elm_ui$Internal$Model$Button),
									A2(
										$elm$core$List$cons,
										$mdgriffith$elm_ui$Internal$Model$Attr(
											$elm$html$Html$Attributes$tabindex(0)),
										function () {
											if (onPress.$ === 1) {
												return A2(
													$elm$core$List$cons,
													$mdgriffith$elm_ui$Internal$Model$Attr(
														$elm$html$Html$Attributes$disabled(true)),
													attrs);
											} else {
												var msg = onPress.a;
												return A2(
													$elm$core$List$cons,
													$mdgriffith$elm_ui$Element$Events$onClick(msg),
													A2(
														$elm$core$List$cons,
														$mdgriffith$elm_ui$Element$Input$onEnter(msg),
														attrs));
											}
										}()))))))),
			$mdgriffith$elm_ui$Internal$Model$Unkeyed(
				_List_fromArray(
					[label])));
	});
var $author$project$Main$buttonFontSize = 24;
var $mdgriffith$elm_ui$Internal$Flag$fontAlignment = $mdgriffith$elm_ui$Internal$Flag$flag(12);
var $mdgriffith$elm_ui$Element$Font$center = A2($mdgriffith$elm_ui$Internal$Model$Class, $mdgriffith$elm_ui$Internal$Flag$fontAlignment, $mdgriffith$elm_ui$Internal$Style$classes.c7);
var $mdgriffith$elm_ui$Internal$Model$Colored = F3(
	function (a, b, c) {
		return {$: 4, a: a, b: b, c: c};
	});
var $mdgriffith$elm_ui$Internal$Model$StyleClass = F2(
	function (a, b) {
		return {$: 4, a: a, b: b};
	});
var $mdgriffith$elm_ui$Internal$Flag$bgColor = $mdgriffith$elm_ui$Internal$Flag$flag(8);
var $mdgriffith$elm_ui$Internal$Model$formatColorClass = function (_v0) {
	var red = _v0.a;
	var green = _v0.b;
	var blue = _v0.c;
	var alpha = _v0.d;
	return $mdgriffith$elm_ui$Internal$Model$floatClass(red) + ('-' + ($mdgriffith$elm_ui$Internal$Model$floatClass(green) + ('-' + ($mdgriffith$elm_ui$Internal$Model$floatClass(blue) + ('-' + $mdgriffith$elm_ui$Internal$Model$floatClass(alpha))))));
};
var $mdgriffith$elm_ui$Element$Background$color = function (clr) {
	return A2(
		$mdgriffith$elm_ui$Internal$Model$StyleClass,
		$mdgriffith$elm_ui$Internal$Flag$bgColor,
		A3(
			$mdgriffith$elm_ui$Internal$Model$Colored,
			'bg-' + $mdgriffith$elm_ui$Internal$Model$formatColorClass(clr),
			'background-color',
			clr));
};
var $mdgriffith$elm_ui$Internal$Model$Fill = function (a) {
	return {$: 2, a: a};
};
var $mdgriffith$elm_ui$Element$fill = $mdgriffith$elm_ui$Internal$Model$Fill(1);
var $mdgriffith$elm_ui$Internal$Model$PaddingStyle = F5(
	function (a, b, c, d, e) {
		return {$: 7, a: a, b: b, c: c, d: d, e: e};
	});
var $mdgriffith$elm_ui$Internal$Flag$padding = $mdgriffith$elm_ui$Internal$Flag$flag(2);
var $mdgriffith$elm_ui$Element$paddingXY = F2(
	function (x, y) {
		return _Utils_eq(x, y) ? A2(
			$mdgriffith$elm_ui$Internal$Model$StyleClass,
			$mdgriffith$elm_ui$Internal$Flag$padding,
			A5(
				$mdgriffith$elm_ui$Internal$Model$PaddingStyle,
				'p-' + $elm$core$String$fromInt(x),
				x,
				x,
				x,
				x)) : A2(
			$mdgriffith$elm_ui$Internal$Model$StyleClass,
			$mdgriffith$elm_ui$Internal$Flag$padding,
			A5(
				$mdgriffith$elm_ui$Internal$Model$PaddingStyle,
				'p-' + ($elm$core$String$fromInt(x) + ('-' + $elm$core$String$fromInt(y))),
				y,
				x,
				y,
				x));
	});
var $mdgriffith$elm_ui$Internal$Flag$borderRound = $mdgriffith$elm_ui$Internal$Flag$flag(17);
var $mdgriffith$elm_ui$Element$Border$rounded = function (radius) {
	return A2(
		$mdgriffith$elm_ui$Internal$Model$StyleClass,
		$mdgriffith$elm_ui$Internal$Flag$borderRound,
		A3(
			$mdgriffith$elm_ui$Internal$Model$Single,
			'br-' + $elm$core$String$fromInt(radius),
			'border-radius',
			$elm$core$String$fromInt(radius) + 'px'));
};
var $mdgriffith$elm_ui$Internal$Model$FontSize = function (a) {
	return {$: 2, a: a};
};
var $mdgriffith$elm_ui$Internal$Flag$fontSize = $mdgriffith$elm_ui$Internal$Flag$flag(4);
var $mdgriffith$elm_ui$Element$Font$size = function (i) {
	return A2(
		$mdgriffith$elm_ui$Internal$Model$StyleClass,
		$mdgriffith$elm_ui$Internal$Flag$fontSize,
		$mdgriffith$elm_ui$Internal$Model$FontSize(i));
};
var $mdgriffith$elm_ui$Internal$Model$Text = function (a) {
	return {$: 2, a: a};
};
var $mdgriffith$elm_ui$Element$text = function (content) {
	return $mdgriffith$elm_ui$Internal$Model$Text(content);
};
var $author$project$Main$button = F3(
	function (msg, lbl, color) {
		return A2(
			$mdgriffith$elm_ui$Element$Input$button,
			_List_fromArray(
				[
					$mdgriffith$elm_ui$Element$Background$color(color),
					A2($mdgriffith$elm_ui$Element$paddingXY, 0, 12),
					$mdgriffith$elm_ui$Element$Font$center,
					$mdgriffith$elm_ui$Element$Font$size($author$project$Main$buttonFontSize),
					$mdgriffith$elm_ui$Element$Border$rounded(6),
					$mdgriffith$elm_ui$Element$width($mdgriffith$elm_ui$Element$fill)
				]),
			{
				ba: $mdgriffith$elm_ui$Element$text(lbl),
				cG: $elm$core$Maybe$Just(msg)
			});
	});
var $author$project$Main$buttonSpacing = 5;
var $mdgriffith$elm_ui$Internal$Model$AsColumn = 1;
var $mdgriffith$elm_ui$Internal$Model$asColumn = 1;
var $mdgriffith$elm_ui$Element$column = F2(
	function (attrs, children) {
		return A4(
			$mdgriffith$elm_ui$Internal$Model$element,
			$mdgriffith$elm_ui$Internal$Model$asColumn,
			$mdgriffith$elm_ui$Internal$Model$div,
			A2(
				$elm$core$List$cons,
				$mdgriffith$elm_ui$Internal$Model$htmlClass($mdgriffith$elm_ui$Internal$Style$classes.cc + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.ae)),
				A2(
					$elm$core$List$cons,
					$mdgriffith$elm_ui$Element$height($mdgriffith$elm_ui$Element$shrink),
					A2(
						$elm$core$List$cons,
						$mdgriffith$elm_ui$Element$width($mdgriffith$elm_ui$Element$shrink),
						attrs))),
			$mdgriffith$elm_ui$Internal$Model$Unkeyed(children));
	});
var $mdgriffith$elm_ui$Internal$Model$Max = F2(
	function (a, b) {
		return {$: 4, a: a, b: b};
	});
var $mdgriffith$elm_ui$Element$maximum = F2(
	function (i, l) {
		return A2($mdgriffith$elm_ui$Internal$Model$Max, i, l);
	});
var $mdgriffith$elm_ui$Internal$Model$Px = function (a) {
	return {$: 0, a: a};
};
var $mdgriffith$elm_ui$Element$px = $mdgriffith$elm_ui$Internal$Model$Px;
var $author$project$Main$columnWidth = F2(
	function (screenSize, w) {
		var max = w - (2 * ($author$project$Main$basePadding.bb + $author$project$Main$basePadding.bt));
		if (!screenSize) {
			return A2(
				$mdgriffith$elm_ui$Element$maximum,
				max,
				$mdgriffith$elm_ui$Element$px(400));
		} else {
			return A2(
				$mdgriffith$elm_ui$Element$maximum,
				max,
				$mdgriffith$elm_ui$Element$px(600));
		}
	});
var $mdgriffith$elm_ui$Internal$Model$Rgba = F4(
	function (a, b, c, d) {
		return {$: 0, a: a, b: b, c: c, d: d};
	});
var $mdgriffith$elm_ui$Element$rgb = F3(
	function (r, g, b) {
		return A4($mdgriffith$elm_ui$Internal$Model$Rgba, r, g, b, 1);
	});
var $author$project$Main$green = A3($mdgriffith$elm_ui$Element$rgb, 0.4, 0.78, 0.4);
var $author$project$Main$purple = A3($mdgriffith$elm_ui$Element$rgb, 0.61, 0.33, 0.88);
var $mdgriffith$elm_ui$Internal$Model$AsRow = 0;
var $mdgriffith$elm_ui$Internal$Model$asRow = 0;
var $mdgriffith$elm_ui$Element$row = F2(
	function (attrs, children) {
		return A4(
			$mdgriffith$elm_ui$Internal$Model$element,
			$mdgriffith$elm_ui$Internal$Model$asRow,
			$mdgriffith$elm_ui$Internal$Model$div,
			A2(
				$elm$core$List$cons,
				$mdgriffith$elm_ui$Internal$Model$htmlClass($mdgriffith$elm_ui$Internal$Style$classes.ae + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.C)),
				A2(
					$elm$core$List$cons,
					$mdgriffith$elm_ui$Element$width($mdgriffith$elm_ui$Element$shrink),
					A2(
						$elm$core$List$cons,
						$mdgriffith$elm_ui$Element$height($mdgriffith$elm_ui$Element$shrink),
						attrs))),
			$mdgriffith$elm_ui$Internal$Model$Unkeyed(children));
	});
var $mdgriffith$elm_ui$Internal$Model$SpacingStyle = F3(
	function (a, b, c) {
		return {$: 5, a: a, b: b, c: c};
	});
var $mdgriffith$elm_ui$Internal$Flag$spacing = $mdgriffith$elm_ui$Internal$Flag$flag(3);
var $mdgriffith$elm_ui$Internal$Model$spacingName = F2(
	function (x, y) {
		return 'spacing-' + ($elm$core$String$fromInt(x) + ('-' + $elm$core$String$fromInt(y)));
	});
var $mdgriffith$elm_ui$Element$spacing = function (x) {
	return A2(
		$mdgriffith$elm_ui$Internal$Model$StyleClass,
		$mdgriffith$elm_ui$Internal$Flag$spacing,
		A3(
			$mdgriffith$elm_ui$Internal$Model$SpacingStyle,
			A2($mdgriffith$elm_ui$Internal$Model$spacingName, x, x),
			x,
			x));
};
var $author$project$Main$teal = A3($mdgriffith$elm_ui$Element$rgb, 0.4, 0.78, 0.8);
var $author$project$Main$buttons = F2(
	function (screenSize, dim) {
		var w = A2($author$project$Main$columnWidth, screenSize, dim.al);
		var group = function () {
			if (!screenSize) {
				return $mdgriffith$elm_ui$Element$column;
			} else {
				return $mdgriffith$elm_ui$Element$row;
			}
		}();
		return A2(
			group,
			_List_fromArray(
				[
					$mdgriffith$elm_ui$Element$spacing($author$project$Main$buttonSpacing),
					$mdgriffith$elm_ui$Element$width(w)
				]),
			_List_fromArray(
				[
					A3($author$project$Main$button, $author$project$Main$Check, 'Check', $author$project$Main$green),
					A3($author$project$Main$button, $author$project$Main$GiveUp, 'Give up', $author$project$Main$purple),
					A3($author$project$Main$button, $author$project$Main$Next, 'Next', $author$project$Main$teal)
				]));
	});
var $author$project$Main$columnSpacing = 30;
var $author$project$Main$Desktop = 1;
var $author$project$Main$Mobile = 0;
var $mdgriffith$elm_ui$Element$BigDesktop = 3;
var $mdgriffith$elm_ui$Element$Desktop = 2;
var $mdgriffith$elm_ui$Element$Landscape = 1;
var $mdgriffith$elm_ui$Element$Phone = 0;
var $mdgriffith$elm_ui$Element$Portrait = 0;
var $mdgriffith$elm_ui$Element$Tablet = 1;
var $mdgriffith$elm_ui$Element$classifyDevice = function (window) {
	return {
		b7: function () {
			var shortSide = A2($elm$core$Basics$min, window.al, window.aG);
			var longSide = A2($elm$core$Basics$max, window.al, window.aG);
			return (shortSide < 600) ? 0 : ((longSide <= 1200) ? 1 : (((longSide > 1200) && (longSide <= 1920)) ? 2 : 3));
		}(),
		cL: (_Utils_cmp(window.al, window.aG) < 0) ? 0 : 1
	};
};
var $author$project$Main$getScreenSize = function (dim) {
	var _v0 = $mdgriffith$elm_ui$Element$classifyDevice(dim);
	var _class = _v0.b7;
	var orientation = _v0.cL;
	var _v1 = _Utils_Tuple2(_class, orientation);
	_v1$2:
	while (true) {
		if (!_v1.b) {
			switch (_v1.a) {
				case 0:
					var _v2 = _v1.a;
					var _v3 = _v1.b;
					return 0;
				case 1:
					var _v4 = _v1.a;
					var _v5 = _v1.b;
					return 0;
				default:
					break _v1$2;
			}
		} else {
			break _v1$2;
		}
	}
	return 1;
};
var $mdgriffith$elm_ui$Internal$Model$AlignX = function (a) {
	return {$: 6, a: a};
};
var $mdgriffith$elm_ui$Internal$Model$Left = 0;
var $mdgriffith$elm_ui$Element$alignLeft = $mdgriffith$elm_ui$Internal$Model$AlignX(0);
var $mdgriffith$elm_ui$Internal$Flag$fontColor = $mdgriffith$elm_ui$Internal$Flag$flag(14);
var $mdgriffith$elm_ui$Element$Font$color = function (fontColor) {
	return A2(
		$mdgriffith$elm_ui$Internal$Model$StyleClass,
		$mdgriffith$elm_ui$Internal$Flag$fontColor,
		A3(
			$mdgriffith$elm_ui$Internal$Model$Colored,
			'fc-' + $mdgriffith$elm_ui$Internal$Model$formatColorClass(fontColor),
			'color',
			fontColor));
};
var $mdgriffith$elm_ui$Element$el = F2(
	function (attrs, child) {
		return A4(
			$mdgriffith$elm_ui$Internal$Model$element,
			$mdgriffith$elm_ui$Internal$Model$asEl,
			$mdgriffith$elm_ui$Internal$Model$div,
			A2(
				$elm$core$List$cons,
				$mdgriffith$elm_ui$Element$width($mdgriffith$elm_ui$Element$shrink),
				A2(
					$elm$core$List$cons,
					$mdgriffith$elm_ui$Element$height($mdgriffith$elm_ui$Element$shrink),
					attrs)),
			$mdgriffith$elm_ui$Internal$Model$Unkeyed(
				_List_fromArray(
					[child])));
	});
var $mdgriffith$elm_ui$Internal$Model$Heading = function (a) {
	return {$: 4, a: a};
};
var $mdgriffith$elm_ui$Element$Region$heading = A2($elm$core$Basics$composeL, $mdgriffith$elm_ui$Internal$Model$Describe, $mdgriffith$elm_ui$Internal$Model$Heading);
var $author$project$Main$headingColor = A3($mdgriffith$elm_ui$Element$rgb, 0.2, 0.34, 0.98);
var $author$project$Main$headingFontSize = 2 * $author$project$Main$baseFontSize;
var $author$project$Main$headingSpacing = 2 * $author$project$Main$baseSpacing;
var $author$project$Main$heading = A2(
	$mdgriffith$elm_ui$Element$el,
	_List_fromArray(
		[
			$mdgriffith$elm_ui$Element$Region$heading(1),
			$mdgriffith$elm_ui$Element$alignLeft,
			$mdgriffith$elm_ui$Element$Font$size($author$project$Main$headingFontSize),
			$mdgriffith$elm_ui$Element$spacing($author$project$Main$headingSpacing),
			$mdgriffith$elm_ui$Element$Font$color($author$project$Main$headingColor)
		]),
	$mdgriffith$elm_ui$Element$text('Jumbles'));
var $mdgriffith$elm_ui$Element$Input$HiddenLabel = function (a) {
	return {$: 1, a: a};
};
var $mdgriffith$elm_ui$Element$Input$labelHidden = $mdgriffith$elm_ui$Element$Input$HiddenLabel;
var $mdgriffith$elm_ui$Internal$Model$OnlyDynamic = F2(
	function (a, b) {
		return {$: 2, a: a, b: b};
	});
var $mdgriffith$elm_ui$Internal$Model$StaticRootAndDynamic = F2(
	function (a, b) {
		return {$: 1, a: a, b: b};
	});
var $mdgriffith$elm_ui$Internal$Model$AllowHover = 1;
var $mdgriffith$elm_ui$Internal$Model$Layout = 0;
var $mdgriffith$elm_ui$Internal$Model$focusDefaultStyle = {
	bU: $elm$core$Maybe$Nothing,
	b_: $elm$core$Maybe$Nothing,
	cW: $elm$core$Maybe$Just(
		{
			R: 0,
			S: A4($mdgriffith$elm_ui$Internal$Model$Rgba, 155 / 255, 203 / 255, 1, 1),
			bg: _Utils_Tuple2(0, 0),
			Z: 3
		})
};
var $mdgriffith$elm_ui$Internal$Model$optionsToRecord = function (options) {
	var combine = F2(
		function (opt, record) {
			switch (opt.$) {
				case 0:
					var hoverable = opt.a;
					var _v4 = record.cp;
					if (_v4.$ === 1) {
						return _Utils_update(
							record,
							{
								cp: $elm$core$Maybe$Just(hoverable)
							});
					} else {
						return record;
					}
				case 1:
					var focusStyle = opt.a;
					var _v5 = record.cl;
					if (_v5.$ === 1) {
						return _Utils_update(
							record,
							{
								cl: $elm$core$Maybe$Just(focusStyle)
							});
					} else {
						return record;
					}
				default:
					var renderMode = opt.a;
					var _v6 = record.cC;
					if (_v6.$ === 1) {
						return _Utils_update(
							record,
							{
								cC: $elm$core$Maybe$Just(renderMode)
							});
					} else {
						return record;
					}
			}
		});
	var andFinally = function (record) {
		return {
			cl: function () {
				var _v0 = record.cl;
				if (_v0.$ === 1) {
					return $mdgriffith$elm_ui$Internal$Model$focusDefaultStyle;
				} else {
					var focusable = _v0.a;
					return focusable;
				}
			}(),
			cp: function () {
				var _v1 = record.cp;
				if (_v1.$ === 1) {
					return 1;
				} else {
					var hoverable = _v1.a;
					return hoverable;
				}
			}(),
			cC: function () {
				var _v2 = record.cC;
				if (_v2.$ === 1) {
					return 0;
				} else {
					var actualMode = _v2.a;
					return actualMode;
				}
			}()
		};
	};
	return andFinally(
		A3(
			$elm$core$List$foldr,
			combine,
			{cl: $elm$core$Maybe$Nothing, cp: $elm$core$Maybe$Nothing, cC: $elm$core$Maybe$Nothing},
			options));
};
var $mdgriffith$elm_ui$Internal$Model$toHtml = F2(
	function (mode, el) {
		switch (el.$) {
			case 0:
				var html = el.a;
				return html($mdgriffith$elm_ui$Internal$Model$asEl);
			case 1:
				var styles = el.a.c4;
				var html = el.a.cq;
				return A2(
					html,
					mode(styles),
					$mdgriffith$elm_ui$Internal$Model$asEl);
			case 2:
				var text = el.a;
				return $mdgriffith$elm_ui$Internal$Model$textElement(text);
			default:
				return $mdgriffith$elm_ui$Internal$Model$textElement('');
		}
	});
var $mdgriffith$elm_ui$Internal$Model$renderRoot = F3(
	function (optionList, attributes, child) {
		var options = $mdgriffith$elm_ui$Internal$Model$optionsToRecord(optionList);
		var embedStyle = function () {
			var _v0 = options.cC;
			if (_v0 === 1) {
				return $mdgriffith$elm_ui$Internal$Model$OnlyDynamic(options);
			} else {
				return $mdgriffith$elm_ui$Internal$Model$StaticRootAndDynamic(options);
			}
		}();
		return A2(
			$mdgriffith$elm_ui$Internal$Model$toHtml,
			embedStyle,
			A4(
				$mdgriffith$elm_ui$Internal$Model$element,
				$mdgriffith$elm_ui$Internal$Model$asEl,
				$mdgriffith$elm_ui$Internal$Model$div,
				attributes,
				$mdgriffith$elm_ui$Internal$Model$Unkeyed(
					_List_fromArray(
						[child]))));
	});
var $mdgriffith$elm_ui$Internal$Model$FontFamily = F2(
	function (a, b) {
		return {$: 1, a: a, b: b};
	});
var $mdgriffith$elm_ui$Internal$Model$SansSerif = {$: 1};
var $mdgriffith$elm_ui$Internal$Model$Typeface = function (a) {
	return {$: 3, a: a};
};
var $mdgriffith$elm_ui$Internal$Flag$fontFamily = $mdgriffith$elm_ui$Internal$Flag$flag(5);
var $elm$core$String$words = _String_words;
var $mdgriffith$elm_ui$Internal$Model$renderFontClassName = F2(
	function (font, current) {
		return _Utils_ap(
			current,
			function () {
				switch (font.$) {
					case 0:
						return 'serif';
					case 1:
						return 'sans-serif';
					case 2:
						return 'monospace';
					case 3:
						var name = font.a;
						return A2(
							$elm$core$String$join,
							'-',
							$elm$core$String$words(
								$elm$core$String$toLower(name)));
					case 4:
						var name = font.a;
						var url = font.b;
						return A2(
							$elm$core$String$join,
							'-',
							$elm$core$String$words(
								$elm$core$String$toLower(name)));
					default:
						var name = font.a.cD;
						return A2(
							$elm$core$String$join,
							'-',
							$elm$core$String$words(
								$elm$core$String$toLower(name)));
				}
			}());
	});
var $mdgriffith$elm_ui$Internal$Model$rootStyle = function () {
	var families = _List_fromArray(
		[
			$mdgriffith$elm_ui$Internal$Model$Typeface('Open Sans'),
			$mdgriffith$elm_ui$Internal$Model$Typeface('Helvetica'),
			$mdgriffith$elm_ui$Internal$Model$Typeface('Verdana'),
			$mdgriffith$elm_ui$Internal$Model$SansSerif
		]);
	return _List_fromArray(
		[
			A2(
			$mdgriffith$elm_ui$Internal$Model$StyleClass,
			$mdgriffith$elm_ui$Internal$Flag$bgColor,
			A3(
				$mdgriffith$elm_ui$Internal$Model$Colored,
				'bg-' + $mdgriffith$elm_ui$Internal$Model$formatColorClass(
					A4($mdgriffith$elm_ui$Internal$Model$Rgba, 1, 1, 1, 0)),
				'background-color',
				A4($mdgriffith$elm_ui$Internal$Model$Rgba, 1, 1, 1, 0))),
			A2(
			$mdgriffith$elm_ui$Internal$Model$StyleClass,
			$mdgriffith$elm_ui$Internal$Flag$fontColor,
			A3(
				$mdgriffith$elm_ui$Internal$Model$Colored,
				'fc-' + $mdgriffith$elm_ui$Internal$Model$formatColorClass(
					A4($mdgriffith$elm_ui$Internal$Model$Rgba, 0, 0, 0, 1)),
				'color',
				A4($mdgriffith$elm_ui$Internal$Model$Rgba, 0, 0, 0, 1))),
			A2(
			$mdgriffith$elm_ui$Internal$Model$StyleClass,
			$mdgriffith$elm_ui$Internal$Flag$fontSize,
			$mdgriffith$elm_ui$Internal$Model$FontSize(20)),
			A2(
			$mdgriffith$elm_ui$Internal$Model$StyleClass,
			$mdgriffith$elm_ui$Internal$Flag$fontFamily,
			A2(
				$mdgriffith$elm_ui$Internal$Model$FontFamily,
				A3($elm$core$List$foldl, $mdgriffith$elm_ui$Internal$Model$renderFontClassName, 'font-', families),
				families))
		]);
}();
var $mdgriffith$elm_ui$Element$layoutWith = F3(
	function (_v0, attrs, child) {
		var options = _v0.bh;
		return A3(
			$mdgriffith$elm_ui$Internal$Model$renderRoot,
			options,
			A2(
				$elm$core$List$cons,
				$mdgriffith$elm_ui$Internal$Model$htmlClass(
					A2(
						$elm$core$String$join,
						' ',
						_List_fromArray(
							[$mdgriffith$elm_ui$Internal$Style$classes.cQ, $mdgriffith$elm_ui$Internal$Style$classes.bS, $mdgriffith$elm_ui$Internal$Style$classes.cX]))),
				_Utils_ap($mdgriffith$elm_ui$Internal$Model$rootStyle, attrs)),
			child);
	});
var $mdgriffith$elm_ui$Element$layout = $mdgriffith$elm_ui$Element$layoutWith(
	{bh: _List_Nil});
var $mdgriffith$elm_ui$Internal$Flag$letterSpacing = $mdgriffith$elm_ui$Internal$Flag$flag(16);
var $mdgriffith$elm_ui$Element$Font$letterSpacing = function (offset) {
	return A2(
		$mdgriffith$elm_ui$Internal$Model$StyleClass,
		$mdgriffith$elm_ui$Internal$Flag$letterSpacing,
		A3(
			$mdgriffith$elm_ui$Internal$Model$Single,
			'ls-' + $mdgriffith$elm_ui$Internal$Model$floatClass(offset),
			'letter-spacing',
			$elm$core$String$fromFloat(offset) + 'px'));
};
var $mdgriffith$elm_ui$Internal$Model$paddingName = F4(
	function (top, right, bottom, left) {
		return 'pad-' + ($elm$core$String$fromInt(top) + ('-' + ($elm$core$String$fromInt(right) + ('-' + ($elm$core$String$fromInt(bottom) + ('-' + $elm$core$String$fromInt(left)))))));
	});
var $mdgriffith$elm_ui$Element$paddingEach = function (_v0) {
	var top = _v0.dm;
	var right = _v0.bt;
	var bottom = _v0.b2;
	var left = _v0.bb;
	return (_Utils_eq(top, right) && (_Utils_eq(top, bottom) && _Utils_eq(top, left))) ? A2(
		$mdgriffith$elm_ui$Internal$Model$StyleClass,
		$mdgriffith$elm_ui$Internal$Flag$padding,
		A5(
			$mdgriffith$elm_ui$Internal$Model$PaddingStyle,
			'p-' + $elm$core$String$fromInt(top),
			top,
			top,
			top,
			top)) : A2(
		$mdgriffith$elm_ui$Internal$Model$StyleClass,
		$mdgriffith$elm_ui$Internal$Flag$padding,
		A5(
			$mdgriffith$elm_ui$Internal$Model$PaddingStyle,
			A4($mdgriffith$elm_ui$Internal$Model$paddingName, top, right, bottom, left),
			top,
			right,
			bottom,
			left));
};
var $mdgriffith$elm_ui$Element$Input$Placeholder = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $mdgriffith$elm_ui$Element$Input$placeholder = $mdgriffith$elm_ui$Element$Input$Placeholder;
var $mdgriffith$elm_ui$Internal$Model$CenterX = 1;
var $mdgriffith$elm_ui$Element$centerX = $mdgriffith$elm_ui$Internal$Model$AlignX(1);
var $author$project$Main$responsiveColumnAttributes = function (screenSize) {
	if (!screenSize) {
		return _List_Nil;
	} else {
		return _List_fromArray(
			[$mdgriffith$elm_ui$Element$centerX]);
	}
};
var $mdgriffith$elm_ui$Element$Input$TextInputNode = function (a) {
	return {$: 0, a: a};
};
var $mdgriffith$elm_ui$Element$Input$TextArea = {$: 1};
var $mdgriffith$elm_ui$Internal$Model$LivePolite = {$: 6};
var $mdgriffith$elm_ui$Element$Region$announce = $mdgriffith$elm_ui$Internal$Model$Describe($mdgriffith$elm_ui$Internal$Model$LivePolite);
var $mdgriffith$elm_ui$Element$Input$applyLabel = F3(
	function (attrs, label, input) {
		if (label.$ === 1) {
			var labelText = label.a;
			return A4(
				$mdgriffith$elm_ui$Internal$Model$element,
				$mdgriffith$elm_ui$Internal$Model$asColumn,
				$mdgriffith$elm_ui$Internal$Model$NodeName('label'),
				attrs,
				$mdgriffith$elm_ui$Internal$Model$Unkeyed(
					_List_fromArray(
						[input])));
		} else {
			var position = label.a;
			var labelAttrs = label.b;
			var labelChild = label.c;
			var labelElement = A4(
				$mdgriffith$elm_ui$Internal$Model$element,
				$mdgriffith$elm_ui$Internal$Model$asEl,
				$mdgriffith$elm_ui$Internal$Model$div,
				labelAttrs,
				$mdgriffith$elm_ui$Internal$Model$Unkeyed(
					_List_fromArray(
						[labelChild])));
			switch (position) {
				case 2:
					return A4(
						$mdgriffith$elm_ui$Internal$Model$element,
						$mdgriffith$elm_ui$Internal$Model$asColumn,
						$mdgriffith$elm_ui$Internal$Model$NodeName('label'),
						attrs,
						$mdgriffith$elm_ui$Internal$Model$Unkeyed(
							_List_fromArray(
								[labelElement, input])));
				case 3:
					return A4(
						$mdgriffith$elm_ui$Internal$Model$element,
						$mdgriffith$elm_ui$Internal$Model$asColumn,
						$mdgriffith$elm_ui$Internal$Model$NodeName('label'),
						attrs,
						$mdgriffith$elm_ui$Internal$Model$Unkeyed(
							_List_fromArray(
								[input, labelElement])));
				case 0:
					return A4(
						$mdgriffith$elm_ui$Internal$Model$element,
						$mdgriffith$elm_ui$Internal$Model$asRow,
						$mdgriffith$elm_ui$Internal$Model$NodeName('label'),
						attrs,
						$mdgriffith$elm_ui$Internal$Model$Unkeyed(
							_List_fromArray(
								[input, labelElement])));
				default:
					return A4(
						$mdgriffith$elm_ui$Internal$Model$element,
						$mdgriffith$elm_ui$Internal$Model$asRow,
						$mdgriffith$elm_ui$Internal$Model$NodeName('label'),
						attrs,
						$mdgriffith$elm_ui$Internal$Model$Unkeyed(
							_List_fromArray(
								[labelElement, input])));
			}
		}
	});
var $elm$html$Html$Attributes$attribute = $elm$virtual_dom$VirtualDom$attribute;
var $mdgriffith$elm_ui$Element$Input$autofill = A2(
	$elm$core$Basics$composeL,
	$mdgriffith$elm_ui$Internal$Model$Attr,
	$elm$html$Html$Attributes$attribute('autocomplete'));
var $mdgriffith$elm_ui$Internal$Model$Behind = 5;
var $mdgriffith$elm_ui$Internal$Model$Nearby = F2(
	function (a, b) {
		return {$: 9, a: a, b: b};
	});
var $mdgriffith$elm_ui$Element$createNearby = F2(
	function (loc, element) {
		if (element.$ === 3) {
			return $mdgriffith$elm_ui$Internal$Model$NoAttribute;
		} else {
			return A2($mdgriffith$elm_ui$Internal$Model$Nearby, loc, element);
		}
	});
var $mdgriffith$elm_ui$Element$behindContent = function (element) {
	return A2($mdgriffith$elm_ui$Element$createNearby, 5, element);
};
var $mdgriffith$elm_ui$Internal$Model$MoveY = function (a) {
	return {$: 1, a: a};
};
var $mdgriffith$elm_ui$Internal$Model$TransformComponent = F2(
	function (a, b) {
		return {$: 10, a: a, b: b};
	});
var $mdgriffith$elm_ui$Internal$Flag$moveY = $mdgriffith$elm_ui$Internal$Flag$flag(26);
var $mdgriffith$elm_ui$Element$moveUp = function (y) {
	return A2(
		$mdgriffith$elm_ui$Internal$Model$TransformComponent,
		$mdgriffith$elm_ui$Internal$Flag$moveY,
		$mdgriffith$elm_ui$Internal$Model$MoveY(-y));
};
var $mdgriffith$elm_ui$Element$Input$calcMoveToCompensateForPadding = function (attrs) {
	var gatherSpacing = F2(
		function (attr, found) {
			if ((attr.$ === 4) && (attr.b.$ === 5)) {
				var _v2 = attr.b;
				var x = _v2.b;
				var y = _v2.c;
				if (found.$ === 1) {
					return $elm$core$Maybe$Just(y);
				} else {
					return found;
				}
			} else {
				return found;
			}
		});
	var _v0 = A3($elm$core$List$foldr, gatherSpacing, $elm$core$Maybe$Nothing, attrs);
	if (_v0.$ === 1) {
		return $mdgriffith$elm_ui$Internal$Model$NoAttribute;
	} else {
		var vSpace = _v0.a;
		return $mdgriffith$elm_ui$Element$moveUp(
			$elm$core$Basics$floor(vSpace / 2));
	}
};
var $mdgriffith$elm_ui$Internal$Flag$overflow = $mdgriffith$elm_ui$Internal$Flag$flag(20);
var $mdgriffith$elm_ui$Element$clip = A2($mdgriffith$elm_ui$Internal$Model$Class, $mdgriffith$elm_ui$Internal$Flag$overflow, $mdgriffith$elm_ui$Internal$Style$classes.b8);
var $mdgriffith$elm_ui$Internal$Flag$borderColor = $mdgriffith$elm_ui$Internal$Flag$flag(28);
var $mdgriffith$elm_ui$Element$Border$color = function (clr) {
	return A2(
		$mdgriffith$elm_ui$Internal$Model$StyleClass,
		$mdgriffith$elm_ui$Internal$Flag$borderColor,
		A3(
			$mdgriffith$elm_ui$Internal$Model$Colored,
			'bc-' + $mdgriffith$elm_ui$Internal$Model$formatColorClass(clr),
			'border-color',
			clr));
};
var $mdgriffith$elm_ui$Element$Input$darkGrey = A3($mdgriffith$elm_ui$Element$rgb, 186 / 255, 189 / 255, 182 / 255);
var $mdgriffith$elm_ui$Element$Input$defaultTextPadding = A2($mdgriffith$elm_ui$Element$paddingXY, 12, 12);
var $mdgriffith$elm_ui$Element$Input$white = A3($mdgriffith$elm_ui$Element$rgb, 1, 1, 1);
var $mdgriffith$elm_ui$Internal$Model$BorderWidth = F5(
	function (a, b, c, d, e) {
		return {$: 6, a: a, b: b, c: c, d: d, e: e};
	});
var $mdgriffith$elm_ui$Element$Border$width = function (v) {
	return A2(
		$mdgriffith$elm_ui$Internal$Model$StyleClass,
		$mdgriffith$elm_ui$Internal$Flag$borderWidth,
		A5(
			$mdgriffith$elm_ui$Internal$Model$BorderWidth,
			'b-' + $elm$core$String$fromInt(v),
			v,
			v,
			v,
			v));
};
var $mdgriffith$elm_ui$Element$Input$defaultTextBoxStyle = _List_fromArray(
	[
		$mdgriffith$elm_ui$Element$Input$defaultTextPadding,
		$mdgriffith$elm_ui$Element$Border$rounded(3),
		$mdgriffith$elm_ui$Element$Border$color($mdgriffith$elm_ui$Element$Input$darkGrey),
		$mdgriffith$elm_ui$Element$Background$color($mdgriffith$elm_ui$Element$Input$white),
		$mdgriffith$elm_ui$Element$Border$width(1),
		$mdgriffith$elm_ui$Element$spacing(5),
		$mdgriffith$elm_ui$Element$width($mdgriffith$elm_ui$Element$fill),
		$mdgriffith$elm_ui$Element$height($mdgriffith$elm_ui$Element$shrink)
	]);
var $mdgriffith$elm_ui$Element$Input$getHeight = function (attr) {
	if (attr.$ === 8) {
		var h = attr.a;
		return $elm$core$Maybe$Just(h);
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $mdgriffith$elm_ui$Internal$Model$Label = function (a) {
	return {$: 5, a: a};
};
var $mdgriffith$elm_ui$Element$Input$hiddenLabelAttribute = function (label) {
	if (label.$ === 1) {
		var textLabel = label.a;
		return $mdgriffith$elm_ui$Internal$Model$Describe(
			$mdgriffith$elm_ui$Internal$Model$Label(textLabel));
	} else {
		return $mdgriffith$elm_ui$Internal$Model$NoAttribute;
	}
};
var $mdgriffith$elm_ui$Internal$Model$InFront = 4;
var $mdgriffith$elm_ui$Element$inFront = function (element) {
	return A2($mdgriffith$elm_ui$Element$createNearby, 4, element);
};
var $mdgriffith$elm_ui$Element$Input$isConstrained = function (len) {
	isConstrained:
	while (true) {
		switch (len.$) {
			case 1:
				return false;
			case 0:
				return true;
			case 2:
				return true;
			case 3:
				var l = len.b;
				var $temp$len = l;
				len = $temp$len;
				continue isConstrained;
			default:
				var l = len.b;
				return true;
		}
	}
};
var $mdgriffith$elm_ui$Element$Input$isHiddenLabel = function (label) {
	if (label.$ === 1) {
		return true;
	} else {
		return false;
	}
};
var $mdgriffith$elm_ui$Element$Input$isStacked = function (label) {
	if (!label.$) {
		var loc = label.a;
		switch (loc) {
			case 0:
				return false;
			case 1:
				return false;
			case 2:
				return true;
			default:
				return true;
		}
	} else {
		return true;
	}
};
var $mdgriffith$elm_ui$Element$Input$negateBox = function (box) {
	return {b2: -box.b2, bb: -box.bb, bt: -box.bt, dm: -box.dm};
};
var $elm$html$Html$Events$alwaysStop = function (x) {
	return _Utils_Tuple2(x, true);
};
var $elm$virtual_dom$VirtualDom$MayStopPropagation = function (a) {
	return {$: 1, a: a};
};
var $elm$html$Html$Events$stopPropagationOn = F2(
	function (event, decoder) {
		return A2(
			$elm$virtual_dom$VirtualDom$on,
			event,
			$elm$virtual_dom$VirtualDom$MayStopPropagation(decoder));
	});
var $elm$json$Json$Decode$at = F2(
	function (fields, decoder) {
		return A3($elm$core$List$foldr, $elm$json$Json$Decode$field, decoder, fields);
	});
var $elm$html$Html$Events$targetValue = A2(
	$elm$json$Json$Decode$at,
	_List_fromArray(
		['target', 'value']),
	$elm$json$Json$Decode$string);
var $elm$html$Html$Events$onInput = function (tagger) {
	return A2(
		$elm$html$Html$Events$stopPropagationOn,
		'input',
		A2(
			$elm$json$Json$Decode$map,
			$elm$html$Html$Events$alwaysStop,
			A2($elm$json$Json$Decode$map, tagger, $elm$html$Html$Events$targetValue)));
};
var $mdgriffith$elm_ui$Element$htmlAttribute = $mdgriffith$elm_ui$Internal$Model$Attr;
var $mdgriffith$elm_ui$Element$Input$isFill = function (len) {
	isFill:
	while (true) {
		switch (len.$) {
			case 2:
				return true;
			case 1:
				return false;
			case 0:
				return false;
			case 3:
				var l = len.b;
				var $temp$len = l;
				len = $temp$len;
				continue isFill;
			default:
				var l = len.b;
				var $temp$len = l;
				len = $temp$len;
				continue isFill;
		}
	}
};
var $mdgriffith$elm_ui$Element$Input$isPixel = function (len) {
	isPixel:
	while (true) {
		switch (len.$) {
			case 1:
				return false;
			case 0:
				return true;
			case 2:
				return false;
			case 3:
				var l = len.b;
				var $temp$len = l;
				len = $temp$len;
				continue isPixel;
			default:
				var l = len.b;
				var $temp$len = l;
				len = $temp$len;
				continue isPixel;
		}
	}
};
var $elm$virtual_dom$VirtualDom$style = _VirtualDom_style;
var $elm$html$Html$Attributes$style = $elm$virtual_dom$VirtualDom$style;
var $mdgriffith$elm_ui$Element$Input$redistributeOver = F4(
	function (isMultiline, stacked, attr, els) {
		switch (attr.$) {
			case 9:
				return _Utils_update(
					els,
					{
						a: A2($elm$core$List$cons, attr, els.a)
					});
			case 7:
				var width = attr.a;
				return $mdgriffith$elm_ui$Element$Input$isFill(width) ? _Utils_update(
					els,
					{
						b: A2($elm$core$List$cons, attr, els.b),
						g: A2($elm$core$List$cons, attr, els.g),
						a: A2($elm$core$List$cons, attr, els.a)
					}) : (stacked ? _Utils_update(
					els,
					{
						b: A2($elm$core$List$cons, attr, els.b)
					}) : _Utils_update(
					els,
					{
						a: A2($elm$core$List$cons, attr, els.a)
					}));
			case 8:
				var height = attr.a;
				return (!stacked) ? _Utils_update(
					els,
					{
						b: A2($elm$core$List$cons, attr, els.b),
						a: A2($elm$core$List$cons, attr, els.a)
					}) : ($mdgriffith$elm_ui$Element$Input$isFill(height) ? _Utils_update(
					els,
					{
						b: A2($elm$core$List$cons, attr, els.b),
						a: A2($elm$core$List$cons, attr, els.a)
					}) : ($mdgriffith$elm_ui$Element$Input$isPixel(height) ? _Utils_update(
					els,
					{
						a: A2($elm$core$List$cons, attr, els.a)
					}) : _Utils_update(
					els,
					{
						a: A2($elm$core$List$cons, attr, els.a)
					})));
			case 6:
				return _Utils_update(
					els,
					{
						b: A2($elm$core$List$cons, attr, els.b)
					});
			case 5:
				return _Utils_update(
					els,
					{
						b: A2($elm$core$List$cons, attr, els.b)
					});
			case 4:
				switch (attr.b.$) {
					case 5:
						var _v1 = attr.b;
						return _Utils_update(
							els,
							{
								b: A2($elm$core$List$cons, attr, els.b),
								g: A2($elm$core$List$cons, attr, els.g),
								a: A2($elm$core$List$cons, attr, els.a),
								ac: A2($elm$core$List$cons, attr, els.ac)
							});
					case 7:
						var cls = attr.a;
						var _v2 = attr.b;
						var pad = _v2.a;
						var t = _v2.b;
						var r = _v2.c;
						var b = _v2.d;
						var l = _v2.e;
						if (isMultiline) {
							return _Utils_update(
								els,
								{
									l: A2($elm$core$List$cons, attr, els.l),
									a: A2($elm$core$List$cons, attr, els.a)
								});
						} else {
							var reducedVerticalPadding = $mdgriffith$elm_ui$Element$paddingEach(
								{
									b2: b - A2($elm$core$Basics$min, t, b),
									bb: l,
									bt: r,
									dm: t - A2($elm$core$Basics$min, t, b)
								});
							var newLineHeight = $mdgriffith$elm_ui$Element$htmlAttribute(
								A2(
									$elm$html$Html$Attributes$style,
									'line-height',
									'calc(1.0em + ' + ($elm$core$String$fromInt(
										2 * A2($elm$core$Basics$min, t, b)) + 'px)')));
							var newHeight = $mdgriffith$elm_ui$Element$htmlAttribute(
								A2(
									$elm$html$Html$Attributes$style,
									'height',
									'calc(1.0em + ' + ($elm$core$String$fromInt(
										2 * A2($elm$core$Basics$min, t, b)) + 'px)')));
							return _Utils_update(
								els,
								{
									l: A2($elm$core$List$cons, attr, els.l),
									g: A2(
										$elm$core$List$cons,
										newHeight,
										A2($elm$core$List$cons, newLineHeight, els.g)),
									a: A2($elm$core$List$cons, reducedVerticalPadding, els.a)
								});
						}
					case 6:
						var _v3 = attr.b;
						return _Utils_update(
							els,
							{
								l: A2($elm$core$List$cons, attr, els.l),
								a: A2($elm$core$List$cons, attr, els.a)
							});
					case 10:
						return _Utils_update(
							els,
							{
								l: A2($elm$core$List$cons, attr, els.l),
								a: A2($elm$core$List$cons, attr, els.a)
							});
					case 2:
						return _Utils_update(
							els,
							{
								b: A2($elm$core$List$cons, attr, els.b)
							});
					case 1:
						var _v4 = attr.b;
						return _Utils_update(
							els,
							{
								b: A2($elm$core$List$cons, attr, els.b)
							});
					default:
						var flag = attr.a;
						var cls = attr.b;
						return _Utils_update(
							els,
							{
								a: A2($elm$core$List$cons, attr, els.a)
							});
				}
			case 0:
				return els;
			case 1:
				var a = attr.a;
				return _Utils_update(
					els,
					{
						g: A2($elm$core$List$cons, attr, els.g)
					});
			case 2:
				return _Utils_update(
					els,
					{
						g: A2($elm$core$List$cons, attr, els.g)
					});
			case 3:
				return _Utils_update(
					els,
					{
						a: A2($elm$core$List$cons, attr, els.a)
					});
			default:
				return _Utils_update(
					els,
					{
						g: A2($elm$core$List$cons, attr, els.g)
					});
		}
	});
var $mdgriffith$elm_ui$Element$Input$redistribute = F3(
	function (isMultiline, stacked, attrs) {
		return function (redist) {
			return {
				l: $elm$core$List$reverse(redist.l),
				b: $elm$core$List$reverse(redist.b),
				g: $elm$core$List$reverse(redist.g),
				a: $elm$core$List$reverse(redist.a),
				ac: $elm$core$List$reverse(redist.ac)
			};
		}(
			A3(
				$elm$core$List$foldl,
				A2($mdgriffith$elm_ui$Element$Input$redistributeOver, isMultiline, stacked),
				{l: _List_Nil, b: _List_Nil, g: _List_Nil, a: _List_Nil, ac: _List_Nil},
				attrs));
	});
var $mdgriffith$elm_ui$Element$Input$renderBox = function (_v0) {
	var top = _v0.dm;
	var right = _v0.bt;
	var bottom = _v0.b2;
	var left = _v0.bb;
	return $elm$core$String$fromInt(top) + ('px ' + ($elm$core$String$fromInt(right) + ('px ' + ($elm$core$String$fromInt(bottom) + ('px ' + ($elm$core$String$fromInt(left) + 'px'))))));
};
var $mdgriffith$elm_ui$Internal$Model$Transparency = F2(
	function (a, b) {
		return {$: 12, a: a, b: b};
	});
var $mdgriffith$elm_ui$Internal$Flag$transparency = $mdgriffith$elm_ui$Internal$Flag$flag(0);
var $mdgriffith$elm_ui$Element$alpha = function (o) {
	var transparency = function (x) {
		return 1 - x;
	}(
		A2(
			$elm$core$Basics$min,
			1.0,
			A2($elm$core$Basics$max, 0.0, o)));
	return A2(
		$mdgriffith$elm_ui$Internal$Model$StyleClass,
		$mdgriffith$elm_ui$Internal$Flag$transparency,
		A2(
			$mdgriffith$elm_ui$Internal$Model$Transparency,
			'transparency-' + $mdgriffith$elm_ui$Internal$Model$floatClass(transparency),
			transparency));
};
var $mdgriffith$elm_ui$Element$Input$charcoal = A3($mdgriffith$elm_ui$Element$rgb, 136 / 255, 138 / 255, 133 / 255);
var $mdgriffith$elm_ui$Element$rgba = $mdgriffith$elm_ui$Internal$Model$Rgba;
var $mdgriffith$elm_ui$Element$Input$renderPlaceholder = F3(
	function (_v0, forPlaceholder, on) {
		var placeholderAttrs = _v0.a;
		var placeholderEl = _v0.b;
		return A2(
			$mdgriffith$elm_ui$Element$el,
			_Utils_ap(
				forPlaceholder,
				_Utils_ap(
					_List_fromArray(
						[
							$mdgriffith$elm_ui$Element$Font$color($mdgriffith$elm_ui$Element$Input$charcoal),
							$mdgriffith$elm_ui$Internal$Model$htmlClass($mdgriffith$elm_ui$Internal$Style$classes.bf + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.cN)),
							$mdgriffith$elm_ui$Element$clip,
							$mdgriffith$elm_ui$Element$Border$color(
							A4($mdgriffith$elm_ui$Element$rgba, 0, 0, 0, 0)),
							$mdgriffith$elm_ui$Element$Background$color(
							A4($mdgriffith$elm_ui$Element$rgba, 0, 0, 0, 0)),
							$mdgriffith$elm_ui$Element$height($mdgriffith$elm_ui$Element$fill),
							$mdgriffith$elm_ui$Element$width($mdgriffith$elm_ui$Element$fill),
							$mdgriffith$elm_ui$Element$alpha(
							on ? 1 : 0)
						]),
					placeholderAttrs)),
			placeholderEl);
	});
var $mdgriffith$elm_ui$Element$scrollbarY = A2($mdgriffith$elm_ui$Internal$Model$Class, $mdgriffith$elm_ui$Internal$Flag$overflow, $mdgriffith$elm_ui$Internal$Style$classes.cU);
var $elm$html$Html$span = _VirtualDom_node('span');
var $elm$html$Html$Attributes$spellcheck = $elm$html$Html$Attributes$boolProperty('spellcheck');
var $mdgriffith$elm_ui$Element$Input$spellcheck = A2($elm$core$Basics$composeL, $mdgriffith$elm_ui$Internal$Model$Attr, $elm$html$Html$Attributes$spellcheck);
var $elm$html$Html$Attributes$type_ = $elm$html$Html$Attributes$stringProperty('type');
var $elm$core$Basics$always = F2(
	function (a, _v0) {
		return a;
	});
var $mdgriffith$elm_ui$Internal$Model$unstyled = A2($elm$core$Basics$composeL, $mdgriffith$elm_ui$Internal$Model$Unstyled, $elm$core$Basics$always);
var $elm$html$Html$Attributes$value = $elm$html$Html$Attributes$stringProperty('value');
var $mdgriffith$elm_ui$Element$Input$value = A2($elm$core$Basics$composeL, $mdgriffith$elm_ui$Internal$Model$Attr, $elm$html$Html$Attributes$value);
var $mdgriffith$elm_ui$Element$Input$textHelper = F3(
	function (textInput, attrs, textOptions) {
		var withDefaults = _Utils_ap($mdgriffith$elm_ui$Element$Input$defaultTextBoxStyle, attrs);
		var redistributed = A3(
			$mdgriffith$elm_ui$Element$Input$redistribute,
			_Utils_eq(textInput.j, $mdgriffith$elm_ui$Element$Input$TextArea),
			$mdgriffith$elm_ui$Element$Input$isStacked(textOptions.ba),
			withDefaults);
		var onlySpacing = function (attr) {
			if ((attr.$ === 4) && (attr.b.$ === 5)) {
				var _v9 = attr.b;
				return true;
			} else {
				return false;
			}
		};
		var heightConstrained = function () {
			var _v7 = textInput.j;
			if (!_v7.$) {
				var inputType = _v7.a;
				return false;
			} else {
				return A2(
					$elm$core$Maybe$withDefault,
					false,
					A2(
						$elm$core$Maybe$map,
						$mdgriffith$elm_ui$Element$Input$isConstrained,
						$elm$core$List$head(
							$elm$core$List$reverse(
								A2($elm$core$List$filterMap, $mdgriffith$elm_ui$Element$Input$getHeight, withDefaults)))));
			}
		}();
		var getPadding = function (attr) {
			if ((attr.$ === 4) && (attr.b.$ === 7)) {
				var cls = attr.a;
				var _v6 = attr.b;
				var pad = _v6.a;
				var t = _v6.b;
				var r = _v6.c;
				var b = _v6.d;
				var l = _v6.e;
				return $elm$core$Maybe$Just(
					{
						b2: A2(
							$elm$core$Basics$max,
							0,
							$elm$core$Basics$floor(b - 3)),
						bb: A2(
							$elm$core$Basics$max,
							0,
							$elm$core$Basics$floor(l - 3)),
						bt: A2(
							$elm$core$Basics$max,
							0,
							$elm$core$Basics$floor(r - 3)),
						dm: A2(
							$elm$core$Basics$max,
							0,
							$elm$core$Basics$floor(t - 3))
					});
			} else {
				return $elm$core$Maybe$Nothing;
			}
		};
		var parentPadding = A2(
			$elm$core$Maybe$withDefault,
			{b2: 0, bb: 0, bt: 0, dm: 0},
			$elm$core$List$head(
				$elm$core$List$reverse(
					A2($elm$core$List$filterMap, getPadding, withDefaults))));
		var inputElement = A4(
			$mdgriffith$elm_ui$Internal$Model$element,
			$mdgriffith$elm_ui$Internal$Model$asEl,
			function () {
				var _v3 = textInput.j;
				if (!_v3.$) {
					var inputType = _v3.a;
					return $mdgriffith$elm_ui$Internal$Model$NodeName('input');
				} else {
					return $mdgriffith$elm_ui$Internal$Model$NodeName('textarea');
				}
			}(),
			_Utils_ap(
				function () {
					var _v4 = textInput.j;
					if (!_v4.$) {
						var inputType = _v4.a;
						return _List_fromArray(
							[
								$mdgriffith$elm_ui$Internal$Model$Attr(
								$elm$html$Html$Attributes$type_(inputType)),
								$mdgriffith$elm_ui$Internal$Model$htmlClass($mdgriffith$elm_ui$Internal$Style$classes.cz)
							]);
					} else {
						return _List_fromArray(
							[
								$mdgriffith$elm_ui$Element$clip,
								$mdgriffith$elm_ui$Element$height($mdgriffith$elm_ui$Element$fill),
								$mdgriffith$elm_ui$Internal$Model$htmlClass($mdgriffith$elm_ui$Internal$Style$classes.cv),
								$mdgriffith$elm_ui$Element$Input$calcMoveToCompensateForPadding(withDefaults),
								$mdgriffith$elm_ui$Element$paddingEach(parentPadding),
								$mdgriffith$elm_ui$Internal$Model$Attr(
								A2(
									$elm$html$Html$Attributes$style,
									'margin',
									$mdgriffith$elm_ui$Element$Input$renderBox(
										$mdgriffith$elm_ui$Element$Input$negateBox(parentPadding)))),
								$mdgriffith$elm_ui$Internal$Model$Attr(
								A2($elm$html$Html$Attributes$style, 'box-sizing', 'content-box'))
							]);
					}
				}(),
				_Utils_ap(
					_List_fromArray(
						[
							$mdgriffith$elm_ui$Element$Input$value(textOptions.c6),
							$mdgriffith$elm_ui$Internal$Model$Attr(
							$elm$html$Html$Events$onInput(textOptions.cE)),
							$mdgriffith$elm_ui$Element$Input$hiddenLabelAttribute(textOptions.ba),
							$mdgriffith$elm_ui$Element$Input$spellcheck(textInput.t),
							A2(
							$elm$core$Maybe$withDefault,
							$mdgriffith$elm_ui$Internal$Model$NoAttribute,
							A2($elm$core$Maybe$map, $mdgriffith$elm_ui$Element$Input$autofill, textInput.p))
						]),
					redistributed.g)),
			$mdgriffith$elm_ui$Internal$Model$Unkeyed(_List_Nil));
		var wrappedInput = function () {
			var _v0 = textInput.j;
			if (_v0.$ === 1) {
				return A4(
					$mdgriffith$elm_ui$Internal$Model$element,
					$mdgriffith$elm_ui$Internal$Model$asEl,
					$mdgriffith$elm_ui$Internal$Model$div,
					_Utils_ap(
						(heightConstrained ? $elm$core$List$cons($mdgriffith$elm_ui$Element$scrollbarY) : $elm$core$Basics$identity)(
							_List_fromArray(
								[
									$mdgriffith$elm_ui$Element$width($mdgriffith$elm_ui$Element$fill),
									A2($elm$core$List$any, $mdgriffith$elm_ui$Element$Input$hasFocusStyle, withDefaults) ? $mdgriffith$elm_ui$Internal$Model$NoAttribute : $mdgriffith$elm_ui$Internal$Model$htmlClass($mdgriffith$elm_ui$Internal$Style$classes.a1),
									$mdgriffith$elm_ui$Internal$Model$htmlClass($mdgriffith$elm_ui$Internal$Style$classes.cy)
								])),
						redistributed.a),
					$mdgriffith$elm_ui$Internal$Model$Unkeyed(
						_List_fromArray(
							[
								A4(
								$mdgriffith$elm_ui$Internal$Model$element,
								$mdgriffith$elm_ui$Internal$Model$asParagraph,
								$mdgriffith$elm_ui$Internal$Model$div,
								A2(
									$elm$core$List$cons,
									$mdgriffith$elm_ui$Element$width($mdgriffith$elm_ui$Element$fill),
									A2(
										$elm$core$List$cons,
										$mdgriffith$elm_ui$Element$height($mdgriffith$elm_ui$Element$fill),
										A2(
											$elm$core$List$cons,
											$mdgriffith$elm_ui$Element$inFront(inputElement),
											A2(
												$elm$core$List$cons,
												$mdgriffith$elm_ui$Internal$Model$htmlClass($mdgriffith$elm_ui$Internal$Style$classes.cx),
												redistributed.ac)))),
								$mdgriffith$elm_ui$Internal$Model$Unkeyed(
									function () {
										if (textOptions.c6 === '') {
											var _v1 = textOptions.cO;
											if (_v1.$ === 1) {
												return _List_fromArray(
													[
														$mdgriffith$elm_ui$Element$text('\u00A0')
													]);
											} else {
												var place = _v1.a;
												return _List_fromArray(
													[
														A3($mdgriffith$elm_ui$Element$Input$renderPlaceholder, place, _List_Nil, textOptions.c6 === '')
													]);
											}
										} else {
											return _List_fromArray(
												[
													$mdgriffith$elm_ui$Internal$Model$unstyled(
													A2(
														$elm$html$Html$span,
														_List_fromArray(
															[
																$elm$html$Html$Attributes$class($mdgriffith$elm_ui$Internal$Style$classes.cw)
															]),
														_List_fromArray(
															[
																$elm$html$Html$text(textOptions.c6 + '\u00A0')
															])))
												]);
										}
									}()))
							])));
			} else {
				var inputType = _v0.a;
				return A4(
					$mdgriffith$elm_ui$Internal$Model$element,
					$mdgriffith$elm_ui$Internal$Model$asEl,
					$mdgriffith$elm_ui$Internal$Model$div,
					A2(
						$elm$core$List$cons,
						$mdgriffith$elm_ui$Element$width($mdgriffith$elm_ui$Element$fill),
						A2(
							$elm$core$List$cons,
							A2($elm$core$List$any, $mdgriffith$elm_ui$Element$Input$hasFocusStyle, withDefaults) ? $mdgriffith$elm_ui$Internal$Model$NoAttribute : $mdgriffith$elm_ui$Internal$Model$htmlClass($mdgriffith$elm_ui$Internal$Style$classes.a1),
							$elm$core$List$concat(
								_List_fromArray(
									[
										redistributed.a,
										function () {
										var _v2 = textOptions.cO;
										if (_v2.$ === 1) {
											return _List_Nil;
										} else {
											var place = _v2.a;
											return _List_fromArray(
												[
													$mdgriffith$elm_ui$Element$behindContent(
													A3($mdgriffith$elm_ui$Element$Input$renderPlaceholder, place, redistributed.l, textOptions.c6 === ''))
												]);
										}
									}()
									])))),
					$mdgriffith$elm_ui$Internal$Model$Unkeyed(
						_List_fromArray(
							[inputElement])));
			}
		}();
		return A3(
			$mdgriffith$elm_ui$Element$Input$applyLabel,
			A2(
				$elm$core$List$cons,
				A2($mdgriffith$elm_ui$Internal$Model$Class, $mdgriffith$elm_ui$Internal$Flag$cursor, $mdgriffith$elm_ui$Internal$Style$classes.ce),
				A2(
					$elm$core$List$cons,
					$mdgriffith$elm_ui$Element$Input$isHiddenLabel(textOptions.ba) ? $mdgriffith$elm_ui$Internal$Model$NoAttribute : $mdgriffith$elm_ui$Element$spacing(5),
					A2($elm$core$List$cons, $mdgriffith$elm_ui$Element$Region$announce, redistributed.b))),
			textOptions.ba,
			wrappedInput);
	});
var $mdgriffith$elm_ui$Element$Input$text = $mdgriffith$elm_ui$Element$Input$textHelper(
	{
		p: $elm$core$Maybe$Nothing,
		t: false,
		j: $mdgriffith$elm_ui$Element$Input$TextInputNode('text')
	});
var $elm$core$String$toUpper = _String_toUpper;
var $author$project$Main$getBody = function (model) {
	var screenSize = $author$project$Main$getScreenSize(model.T);
	return A2(
		$mdgriffith$elm_ui$Element$layout,
		_List_fromArray(
			[
				$mdgriffith$elm_ui$Element$Font$size($author$project$Main$baseFontSize),
				$mdgriffith$elm_ui$Element$paddingEach($author$project$Main$basePadding)
			]),
		A2(
			$mdgriffith$elm_ui$Element$column,
			_Utils_ap(
				_List_fromArray(
					[
						$mdgriffith$elm_ui$Element$width(
						A2($author$project$Main$columnWidth, screenSize, model.T.al)),
						$mdgriffith$elm_ui$Element$spacing($author$project$Main$columnSpacing)
					]),
				$author$project$Main$responsiveColumnAttributes(screenSize)),
			_List_fromArray(
				[
					$author$project$Main$heading,
					A2(
					$mdgriffith$elm_ui$Element$row,
					_List_fromArray(
						[
							$mdgriffith$elm_ui$Element$spacing($author$project$Main$baseSpacing),
							$mdgriffith$elm_ui$Element$Font$letterSpacing(2)
						]),
					_List_fromArray(
						[
							$mdgriffith$elm_ui$Element$text(
							$elm$core$String$toUpper(model.au))
						])),
					A2(
					$mdgriffith$elm_ui$Element$Input$text,
					_List_fromArray(
						[
							$mdgriffith$elm_ui$Element$spacing($author$project$Main$baseSpacing)
						]),
					{
						ba: $mdgriffith$elm_ui$Element$Input$labelHidden('Guess'),
						cE: $author$project$Main$GuessChanged,
						cO: $elm$core$Maybe$Just(
							A2(
								$mdgriffith$elm_ui$Element$Input$placeholder,
								_List_Nil,
								$mdgriffith$elm_ui$Element$text('Guess'))),
						c6: model.V
					}),
					A2($author$project$Main$buttons, screenSize, model.T),
					A2(
					$mdgriffith$elm_ui$Element$row,
					_List_fromArray(
						[
							$mdgriffith$elm_ui$Element$spacing($author$project$Main$baseSpacing)
						]),
					_List_fromArray(
						[
							$mdgriffith$elm_ui$Element$text(model.w)
						])),
					A2(
					$mdgriffith$elm_ui$Element$row,
					_List_fromArray(
						[
							$mdgriffith$elm_ui$Element$spacing($author$project$Main$baseSpacing)
						]),
					_List_fromArray(
						[
							$mdgriffith$elm_ui$Element$text(
							'Score: ' + $elm$core$String$fromInt(model.Y))
						]))
				])));
};
var $author$project$Main$view = function (model) {
	return {
		bY: _List_fromArray(
			[
				$author$project$Main$getBody(model)
			]),
		dl: 'Jumbles'
	};
};
var $author$project$Main$main = $elm$browser$Browser$document(
	{cu: $author$project$Main$init, c5: $author$project$Main$subscriptions, dq: $author$project$Main$update, dr: $author$project$Main$view});
_Platform_export({'Main':{'init':$author$project$Main$main($elm$json$Json$Decode$value)(0)}});}(this));