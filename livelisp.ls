kLPar = '('
kRPar = ')'
kQuote = "'"
kNil = { tag: 'nil', data: 'nil' }

safeCar = (obj) ->
  if obj.tag is 'cons'
    obj.car
  else
    kNil

safeCdr = (obj) ->
  if obj.tag is 'cons'
    obj.cdr
  else
    kNil

makeError = (str) ->
  { tag: 'error', data: str }

sym_table = {}
makeSym = (str) ->
  if str is 'nil'
    return kNil
  if not sym_table[str]
    sym_table[str] = { tag: 'sym', data: str }
  return sym_table[str]

makeNum = (num) ->
  { tag: 'num', data: num }

makeCons = (a, d) ->
  { tag: 'cons', car: a, cdr: d }

makeSubr = (fn) ->
  { tag: 'subr', data: fn }

makeExpr = (args, env) ->
  { tag: 'expr', args: safeCar(args), body: safeCdr(args), env: env }

nreverse = (lst) ->
  ret = kNil
  while lst.tag is 'cons'
    tmp = lst.cdr
    lst.cdr = ret
    ret = lst
    lst = tmp
  ret

pairlis = (lst1, lst2) ->
  ret = kNil
  while lst1.tag is 'cons' and lst2.tag is 'cons'
    ret = makeCons(makeCons(lst1.car, lst2.car), ret)
    lst1 = lst1.cdr
    lst2 = lst2.cdr
  nreverse(ret)

isDelimiter = (c) ->
  c is kLPar or c is kRPar or c is kQuote or /\s+/.test(c)

skipSpaces = (str) ->
  str.replace(/^\s+/, '')

makeNumOrSym = (str) ->
  num = parseInt(str, 10)
  if str is num.toString()
    makeNum(num)
  else
    makeSym(str)

readAtom = (str) ->
  next = ''
  for i in [0 til str.length]
    if isDelimiter(str[i])
      next = str.substring(i)
      str = str.substring(0, i)
      break
  [makeNumOrSym(str), next]

read = (str) ->
  str = skipSpaces(str)
  if str.length is 0
    makeError('empty input')
  else if str[0] is kRPar
    makeError('invalid syntax: ' + str)
  else if str[0] is kLPar
    readList(str.substring(1))
  else if str[0] is kQuote
    [elm, next] = read(str.substring(1))
    [makeCons(makeSym('quote'), makeCons(elm, kNil)), next]
  else
    readAtom(str)

readList = (str) ->
  ret = kNil
  while true
    str = skipSpaces(str)
    if str.length is 0
      return [makeError('unfinished parenthesis'), '']
    else if str[0] is kRPar
      break
    [elm, next] = read(str)
    if elm.tag is 'error'
      return [elm, '']
    ret = makeCons(elm, ret)
    str = next
  [nreverse(ret), str.substring(1)]

printObj = (obj) ->
  if obj.tag is 'num' or obj.tag is 'sym' or obj.tag is 'nil'
    obj.data.toString()
  else if obj.tag is 'error'
    '<error: ' + obj.data + '>'
  else if obj.tag is 'cons'
    printList(obj)
  else if obj.tag is 'subr' or obj.tag is 'expr'
    '<' + obj.tag + '>'
  else
    '<unknown>'

printList = (obj) ->
  ret = ''
  first = true
  while obj.tag is 'cons'
    if first
      first = false
    else
      ret += ' '
    ret += printObj(obj.car)
    obj = obj.cdr
  if obj.tag is 'nil'
    '(' + ret + ')'
  else
    '(' + ret + ' . ' + printObj(obj) + ')'

findVar = (sym, env) ->
  while env.tag is 'cons'
    alist = env.car
    while alist.tag is 'cons'
      if alist.car.car is sym
        return alist.car
      alist = alist.cdr
    env = env.cdr
  kNil

g_env = makeCons(kNil, kNil)

addToEnv = (sym, val, env) ->
  env.car = makeCons(makeCons(sym, val), env.car)

eval1 = (obj, env) ->
  if obj.tag is 'nil' or obj.tag is 'num' or obj.tag is 'error'
    return obj
  else if obj.tag is 'sym'
    bind = findVar(obj, env)
    if bind is kNil
      return makeError(obj.data + ' has no value')
    return bind.cdr

  op = safeCar(obj)
  args = safeCdr(obj)
  if op is makeSym('quote')
    return safeCar(args)
  else if op is makeSym('if')
    if eval1(safeCar(args), env) is kNil
      return eval1(safeCar(safeCdr(safeCdr(args))), env)
    return eval1(safeCar(safeCdr(args)), env)
  else if op is makeSym('lambda')
    return makeExpr(args, env)
  else if op is makeSym('defun')
    expr = makeExpr(safeCdr(args), env)
    sym = safeCar(args)
    addToEnv(sym, expr, g_env)
    return sym
  else if op is makeSym('setq')
    val = eval1(safeCar(safeCdr(args)), env)
    sym = safeCar(args)
    bind = findVar(sym, env)
    if bind is kNil
      addToEnv(sym, val, g_env)
    else
      bind.cdr = val
    return val
  apply(eval1(op, env), evlis(args, env), env)

evlis = (lst, env) ->
  ret = kNil
  while lst.tag is 'cons'
    elm = eval1(lst.car, env)
    if elm.tag is 'error'
      return elm
    ret = makeCons(elm, ret)
    lst = lst.cdr
  nreverse(ret)

progn = (body, env) ->
  ret = kNil
  while body.tag is 'cons'
    ret = eval1(body.car, env)
    body = body.cdr
  ret

apply = (fn, args, env) ->
  if fn.tag is 'error'
    fn
  else if args.tag is 'error'
    args
  else if fn.tag is 'subr'
    fn.data(args)
  else if fn.tag is 'expr'
    progn(fn.body, makeCons(pairlis(fn.args, args), fn.env))
  else
    makeError('noimpl')

subrCar = (args) ->
  safeCar(safeCar(args))

subrCdr = (args) ->
  safeCdr(safeCar(args))

subrCons = (args) ->
  makeCons(safeCar(args), safeCar(safeCdr(args)))

subrEq = (args) ->
  x = safeCar(args)
  y = safeCar(safeCdr(args))
  if x.tag is 'num' and y.tag is 'num'
    if x.data is y.data
      makeSym('t')
    else
      kNil
  else if x is y
    makeSym('t')
  else
    kNil

subrAtom = (args) ->
  if safeCar(args).tag is 'cons'
    kNil
  else
    makeSym('t')

subrNumberp = (args) ->
  if safeCar(args).tag is 'num'
    makeSym('t')
  else
    kNil

subrSymbolp = (args) ->
  if safeCar(args).tag is 'sym'
    makeSym('t')
  else
    kNil

subrAddOrMul = (fn, init_val) ->
  (args) ->
    ret = init_val
    while args.tag is 'cons'
      if args.car.tag isnt 'num'
        return makeError('wrong type')
      ret = fn(ret, args.car.data)
      args = args.cdr
    makeNum(ret)
subrAdd = subrAddOrMul(((x, y) -> x + y), 0)
subrMul = subrAddOrMul(((x, y) -> x * y), 1)

subrSubOrDivOrMod = (fn) ->
  (args) ->
    x = safeCar(args)
    y = safeCar(safeCdr(args))
    if x.tag isnt 'num' or y.tag isnt 'num'
      return makeError('wrong type')
    makeNum(fn(x.data, y.data))
subrSub = subrSubOrDivOrMod((x, y) -> x - y)
subrDiv = subrSubOrDivOrMod((x, y) -> x / y)
subrMod = subrSubOrDivOrMod((x, y) -> x % y)

addToEnv(makeSym('car'), makeSubr(subrCar), g_env)
addToEnv(makeSym('cdr'), makeSubr(subrCdr), g_env)
addToEnv(makeSym('cons'), makeSubr(subrCons), g_env)
addToEnv(makeSym('eq'), makeSubr(subrEq), g_env)
addToEnv(makeSym('atom'), makeSubr(subrAtom), g_env)
addToEnv(makeSym('numberp'), makeSubr(subrNumberp), g_env)
addToEnv(makeSym('symbolp'), makeSubr(subrSymbolp), g_env)
addToEnv(makeSym('+'), makeSubr(subrAdd), g_env)
addToEnv(makeSym('*'), makeSubr(subrMul), g_env)
addToEnv(makeSym('-'), makeSubr(subrSub), g_env)
addToEnv(makeSym('/'), makeSubr(subrDiv), g_env)
addToEnv(makeSym('mod'), makeSubr(subrMod), g_env)
addToEnv(makeSym('t'), makeSym('t'), g_env)

stdin = process.openStdin()
stdin.setEncoding 'utf8'
process.stdout.write('> ')
stdin.on 'data', (input) ->
  console.log(printObj(eval1(read(input)[0], g_env)))
  process.stdout.write('> ')
