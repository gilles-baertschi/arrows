declare i32 @printf(i8*, ...)
declare ptr @malloc(i64)
declare i8 @getchar()

@formatStringChar = private constant [2 x i8] c"%c"

define void @putChar(i8 %char) {
  call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @formatStringChar , i32 0, i32 0), i8 %char)
  ret void
}

define i64 @readChar() {
  %res.0 = call i8 @getchar()
  %res.1 = zext i8 %res.0 to i64
  ret i64 %res.1
}

define i64 @id(i64 %arg) {
  ret i64 %arg
}

define ptr @double(i64 %arg) {
  %res = call {i64, i64}* @malloc(i64 16)
  %first = getelementptr {i64, i64}, ptr %res, i64 0, i32 0
  %second = getelementptr {i64, i64}, ptr %res, i64 0, i32 1
  store i64 %arg, i64* %first
  store i64 %arg, i64* %second
  ret ptr %res
}

define ptr @l(i64 %arg) {
  %res = call {i64, i64}* @malloc(i64 16)
  %first = getelementptr {i64, i64}, ptr %res, i64 0, i32 0
  %second = getelementptr {i64, i64}, ptr %res, i64 0, i32 1
  store i64 0, i64* %first
  store i64 %arg, i64* %second
  ret ptr %res
}

define ptr @r(i64 %arg) {
  %res = call {i64, i64}* @malloc(i64 16)
  %first = getelementptr {i64, i64}, ptr %res, i64 0, i32 0
  %second = getelementptr {i64, i64}, ptr %res, i64 0, i32 1
  store i64 1, i64* %first
  store i64 %arg, i64* %second
  ret ptr %res
}

define ptr @swap(ptr %arg) {
  %pair = load {i64, i64}, ptr %arg
  %v1 = extractvalue {i64, i64} %pair, 0
  %v2 = extractvalue {i64, i64} %pair, 1
  %res = call {i64, i64}* @malloc(i64 16)
  %first = getelementptr {i64, i64}, ptr %res, i64 0, i32 0
  %second = getelementptr {i64, i64}, ptr %res, i64 0, i32 1
  store i64 %v2, i64* %first
  store i64 %v1, i64* %second
  ret ptr %res
}

define ptr @swapChoice(ptr %arg) {
  %pair = load {i64, i64}, ptr %arg
  %v1.old = extractvalue {i64, i64} %pair, 0
  %v2 = extractvalue {i64, i64} %pair, 1
  %res = call {i64, i64}* @malloc(i64 16)
  %first = getelementptr {i64, i64}, ptr %res, i64 0, i32 0
  %second = getelementptr {i64, i64}, ptr %res, i64 0, i32 1
  %v1.new = xor i64 %v1.old, 1
  store i64 %v1.new, i64* %first
  store i64 %v2, i64* %second
  ret ptr %res
}

define i64 @addInt(ptr %arg) {
  %pair = load {i64, i64}, ptr %arg
  %v1 = extractvalue {i64, i64} %pair, 0
  %v2 = extractvalue {i64, i64} %pair, 1
  %res = add i64 %v1, %v2
  ret i64 %res
}

define i64 @subInt(ptr %arg) {
  %pair = load {i64, i64}, ptr %arg
  %v1 = extractvalue {i64, i64} %pair, 0
  %v2 = extractvalue {i64, i64} %pair, 1
  %res = sub i64 %v1, %v2
  ret i64 %res
}

define i64 @mulInt(ptr %arg) {
  %pair = load {i64, i64}, ptr %arg
  %v1 = extractvalue {i64, i64} %pair, 0
  %v2 = extractvalue {i64, i64} %pair, 1
  %res = mul i64 %v1, %v2
  ret i64 %res
}

define i64 @divInt(ptr %arg) {
  %pair = load {i64, i64}, ptr %arg
  %v1 = extractvalue {i64, i64} %pair, 0
  %v2 = extractvalue {i64, i64} %pair, 1
  %res = sdiv i64 %v1, %v2
  ret i64 %res
}

define i64 @mod(ptr %arg) {
  %pair = load {i64, i64}, ptr %arg
  %v1 = extractvalue {i64, i64} %pair, 0
  %v2 = extractvalue {i64, i64} %pair, 1
  %res = srem i64 %v1, %v2
  ret i64 %res
}

define double @addFloat(ptr %arg) {
  %pair = load {double, double}, ptr %arg
  %v1 = extractvalue {double, double} %pair, 0
  %v2 = extractvalue {double, double} %pair, 1
  %res = fadd double %v1, %v2
  ret double %res
}

define double @subFloat(ptr %arg) {
  %pair = load {double, double}, ptr %arg
  %v1 = extractvalue {double, double} %pair, 0
  %v2 = extractvalue {double, double} %pair, 1
  %res = fsub double %v1, %v2
  ret double %res
}

define double @mulFloat(ptr %arg) {
  %pair = load {double, double}, ptr %arg
  %v1 = extractvalue {double, double} %pair, 0
  %v2 = extractvalue {double, double} %pair, 1
  %res = fmul double %v1, %v2
  ret double %res
}

define double @divFloat(ptr %arg) {
  %pair = load {double, double}, ptr %arg
  %v1 = extractvalue {double, double} %pair, 0
  %v2 = extractvalue {double, double} %pair, 1
  %res = fdiv double %v1, %v2
  ret double %res
}

define i64 @or(ptr %arg) {
  %pair = load {i64, i64}, ptr %arg
  %v1 = extractvalue {i64, i64} %pair, 0
  %v2 = extractvalue {i64, i64} %pair, 1
  %res = or i64 %v1, %v2
  ret i64 %res
}

define i64 @xor(ptr %arg) {
  %pair = load {i64, i64}, ptr %arg
  %v1 = extractvalue {i64, i64} %pair, 0
  %v2 = extractvalue {i64, i64} %pair, 1
  %res = xor i64 %v1, %v2
  ret i64 %res
}

define i64 @and(ptr %arg) {
  %pair = load {i64, i64}, ptr %arg
  %v1 = extractvalue {i64, i64} %pair, 0
  %v2 = extractvalue {i64, i64} %pair, 1
  %res = and i64 %v1, %v2
  ret i64 %res
}

define i64 @not(i64 %arg) {
  %res = xor i64 %arg, 1
  ret i64 %res
}

define i64 @negInt(i64 %arg) {
  %res = mul i64 %arg, -1
  ret i64 %res
}

define i64 @absInt(i64 %arg) {
  %sign = and i64 %arg, u0x8000000000000000
  %negative = icmp eq i64 %sign, 0
  br i1 %negative, label %pos, label %neg
neg:
  %res = mul i64 %arg, -1
  ret i64 %res
pos:
  ret i64 %arg
}

define double @negFloat(double %arg) {
  %res = fmul double %arg, -1.0
  ret double %res
}

define double @absFloat(double %arg) {
  %sign = fcmp oge double %arg, 0.0
  br i1 %sign, label %pos, label %neg
neg:
  %res = fmul double %arg, -1.0
  ret double %res
pos:
  ret double %arg
}

define i64 @floatToInt(double %arg) {
  %res = fptosi double %arg to i64
  ret i64 %res
}

define i64 @eq(ptr %arg) {
  %pair = load {i64, i64}, ptr %arg
  %v1 = extractvalue {i64, i64} %pair, 0
  %v2 = extractvalue {i64, i64} %pair, 1
  %res.0 = icmp eq i64 %v1, %v2
  %res.1 = zext i1 %res.0 to i64
  ret i64 %res.1
}

define i64 @greaterInt(ptr %arg) {
  %pair = load {i64, i64}, ptr %arg
  %v1 = extractvalue {i64, i64} %pair, 0
  %v2 = extractvalue {i64, i64} %pair, 1
  %res.0 = icmp sgt i64 %v1, %v2
  %res.1 = zext i1 %res.0 to i64
  ret i64 %res.1
}

define i64 @greaterEqInt(ptr %arg) {
  %pair = load {i64, i64}, ptr %arg
  %v1 = extractvalue {i64, i64} %pair, 0
  %v2 = extractvalue {i64, i64} %pair, 1
  %res.0 = icmp sge i64 %v1, %v2
  %res.1 = zext i1 %res.0 to i64
  ret i64 %res.1
}

define i64 @lessInt(ptr %arg) {
  %pair = load {i64, i64}, ptr %arg
  %v1 = extractvalue {i64, i64} %pair, 0
  %v2 = extractvalue {i64, i64} %pair, 1
  %res.0 = icmp slt i64 %v1, %v2
  %res.1 = zext i1 %res.0 to i64
  ret i64 %res.1
}

define i64 @lessEqInt(ptr %arg) {
  %pair = load {i64, i64}, ptr %arg
  %v1 = extractvalue {i64, i64} %pair, 0
  %v2 = extractvalue {i64, i64} %pair, 1
  %res.0 = icmp sle i64 %v1, %v2
  %res.1 = zext i1 %res.0 to i64
  ret i64 %res.1
}

define i64 @eqFloat(ptr %arg) {
  %pair = load {double, double}, ptr %arg
  %v1 = extractvalue {double, double} %pair, 0
  %v2 = extractvalue {double, double} %pair, 1
  %res.0 = fcmp oeq double %v1, %v2
  %res.1 = zext i1 %res.0 to i64
  ret i64 %res.1
}

define i64 @greaterFloat(ptr %arg) {
  %pair = load {double, double}, ptr %arg
  %v1 = extractvalue {double, double} %pair, 0
  %v2 = extractvalue {double, double} %pair, 1
  %res.0 = fcmp ogt double %v1, %v2
  %res.1 = zext i1 %res.0 to i64
  ret i64 %res.1
}

define i64 @greaterEqFloat(ptr %arg) {
  %pair = load {double, double}, ptr %arg
  %v1 = extractvalue {double, double} %pair, 0
  %v2 = extractvalue {double, double} %pair, 1
  %res.0 = fcmp oge double %v1, %v2
  %res.1 = zext i1 %res.0 to i64
  ret i64 %res.1
}

define i64 @lessFloat(ptr %arg) {
  %pair = load {double, double}, ptr %arg
  %v1 = extractvalue {double, double} %pair, 0
  %v2 = extractvalue {double, double} %pair, 1
  %res.0 = fcmp olt double %v1, %v2
  %res.1 = zext i1 %res.0 to i64
  ret i64 %res.1
}

define i64 @lessEqFloat(ptr %arg) {
  %pair = load {double, double}, ptr %arg
  %v1 = extractvalue {double, double} %pair, 0
  %v2 = extractvalue {double, double} %pair, 1
  %res.0 = fcmp ole double %v1, %v2
  %res.1 = zext i1 %res.0 to i64
  ret i64 %res.1
}

define i64 @fst(ptr %arg) {
  %pair = load {i64, i64}, ptr %arg
  %v = extractvalue {i64, i64} %pair, 0
  ret i64 %v
}

define i64 @snd(ptr %arg) {
  %pair = load {i64, i64}, ptr %arg
  %v = extractvalue {i64, i64} %pair, 1
  ret i64 %v
}

define ptr @reorderToFront(ptr %arg) {
  %pair.outer = load {i64, ptr}, ptr %arg
  %v1 = extractvalue {i64, ptr}  %pair.outer, 0
  %arg.inner = extractvalue {i64, ptr} %pair.outer, 1
  %pair.inner = load {i64, i64}, ptr %arg.inner
  %v2 = extractvalue {i64, i64}  %pair.inner, 0
  %v3 = extractvalue {i64, i64} %pair.inner, 1
  %res.inner = call {i64, i64}* @malloc(i64 16)
  %res.outer = call {ptr, i64}* @malloc(i64 16)
  %first.inner = getelementptr {i64, i64}, ptr %res.inner, i64 0, i32 0
  %second.inner = getelementptr {i64, i64}, ptr %res.inner, i64 0, i32 1
  %first.outer = getelementptr {ptr, i64}, ptr %res.outer, i64 0, i32 0
  %second.outer = getelementptr {ptr, i64}, ptr %res.outer, i64 0, i32 1
  store i64 %v1, i64* %first.inner
  store i64 %v2, i64* %second.inner
  store ptr %res.inner, ptr %first.outer
  store i64 %v3, i64* %second.outer
  ret ptr %res.outer
}

define ptr @reorderToBack(ptr %arg) {
  %pair.outer = load {ptr, i64}, ptr %arg
  %arg.inner = extractvalue {ptr, i64} %pair.outer, 0
  %pair.inner = load {i64, i64}, ptr %arg.inner
  %v1 = extractvalue {i64, i64}  %pair.inner, 0
  %v2 = extractvalue {i64, i64} %pair.inner, 1
  %v3 = extractvalue {ptr, i64}  %pair.outer, 1
  %res.inner = call {i64, i64}* @malloc(i64 16)
  %res.outer = call {ptr, i64}* @malloc(i64 16)
  %first.inner = getelementptr {i64, i64}, ptr %res.inner, i64 0, i32 0
  %second.inner = getelementptr {i64, i64}, ptr %res.inner, i64 0, i32 1
  %first.outer = getelementptr {ptr, i64}, ptr %res.outer, i64 0, i32 0
  %second.outer = getelementptr {ptr, i64}, ptr %res.outer, i64 0, i32 1
  store i64 %v2, i64* %first.inner
  store i64 %v3, i64* %second.inner
  store i64 %v1, i64* %first.outer
  store ptr %res.inner, ptr %second.outer
  ret ptr %res.outer
}

define ptr @includeLeft(ptr %arg) {
  %pair.outer = load {i64, ptr}, ptr %arg
  %v1 = extractvalue {i64, ptr}  %pair.outer, 0
  %arg.inner = extractvalue {i64, ptr} %pair.outer, 1
  %pair.inner = load {i64, i64}, ptr %arg.inner
  %choice = extractvalue {i64, i64}  %pair.inner, 0
  %v2 = extractvalue {i64, i64} %pair.inner, 1
  %res.inner = call {i64, i64}* @malloc(i64 16)
  %res.outer = call {ptr, i64}* @malloc(i64 16)
  %first.inner = getelementptr {i64, i64}, ptr %res.inner, i64 0, i32 0
  %second.inner = getelementptr {i64, i64}, ptr %res.inner, i64 0, i32 1
  %first.outer = getelementptr {ptr, i64}, ptr %res.outer, i64 0, i32 0
  %second.outer = getelementptr {ptr, i64}, ptr %res.outer, i64 0, i32 1
  store i64 %v1, i64* %first.inner
  store i64 %v2, i64* %second.inner
  store i64 %choice, i64* %first.outer
  store ptr %res.inner, ptr %second.outer
  ret ptr %res.outer
}

define ptr @includeRight(ptr %arg) {
  %pair.outer = load {ptr, i64}, ptr %arg
  %arg.inner = extractvalue {ptr, i64} %pair.outer, 0
  %pair.inner = load {i64, i64}, ptr %arg.inner
  %choice = extractvalue {i64, i64}  %pair.inner, 0
  %v1 = extractvalue {i64, i64} %pair.inner, 1
  %v2 = extractvalue {ptr, i64}  %pair.outer, 1
  %res.inner = call {i64, i64}* @malloc(i64 16)
  %res.outer = call {ptr, i64}* @malloc(i64 16)
  %first.inner = getelementptr {i64, i64}, ptr %res.inner, i64 0, i32 0
  %second.inner = getelementptr {i64, i64}, ptr %res.inner, i64 0, i32 1
  %first.outer = getelementptr {ptr, i64}, ptr %res.outer, i64 0, i32 0
  %second.outer = getelementptr {ptr, i64}, ptr %res.outer, i64 0, i32 1
  store i64 %v1, i64* %first.inner
  store i64 %v2, i64* %second.inner
  store i64 %choice, i64* %first.outer
  store ptr %res.inner, ptr %second.outer
  ret ptr %res.outer
}

; includeLeft :: (c, (a | b)) -> ((c, a) | (c, b)) 
; includeRight :: ((a | b), c) -> ((a, c) | (b, c)) 

define i64 @app(ptr %arg) {
  %pair = load {ptr, i64}, ptr %arg
  %fp = extractvalue {ptr, i64} %pair, 0
  %v = extractvalue {ptr, i64} %pair, 1
  %res = call i64 @callExtended(i64 %v, ptr %fp)
  ret i64 %res
}

define ptr @composition(i64 %arg) {
  %res = call {i64, i64, ptr}* @malloc(i64 24)
  %zero = getelementptr {i64, i64, ptr}, ptr %res, i64 0, i32 0
  %arg.extended = getelementptr {i64, i64, ptr}, ptr %res, i64 0, i32 1
  %fp.extended = getelementptr {i64, i64, ptr}, ptr %res, i64 0, i32 2
  store i64 0, i64* %zero
  store i64 %arg, i64* %arg.extended
  store ptr @compositionInner, ptr %fp.extended
  ret ptr %res
}

define i64 @compositionInner(i64 %arg, ptr %fps) {
  %pair = load {ptr, ptr}, ptr %fps
  %fp1 = extractvalue {ptr, ptr} %pair, 0
  %fp2 = extractvalue {ptr, ptr} %pair, 1
  %mid = call i64 @callExtended(i64 %arg, ptr %fp1)
  %res = call i64 @callExtended(i64 %mid, ptr %fp2)
  ret i64 %res
}

define ptr @first(i64 %arg) {
  %res = call {i64, i64, ptr}* @malloc(i64 24)
  %zero = getelementptr {i64, i64, ptr}, ptr %res, i64 0, i32 0
  %arg.extended = getelementptr {i64, i64, ptr}, ptr %res, i64 0, i32 1
  %fp.extended = getelementptr {i64, i64, ptr}, ptr %res, i64 0, i32 2
  store i64 0, i64* %zero
  store i64 %arg, i64* %arg.extended
  store ptr @firstInner, ptr %fp.extended
  ret ptr %res
}

define ptr @firstInner(ptr %arg, ptr %fp) {
  %pair = load {i64, i64}, ptr %arg
  %v1.old = extractvalue {i64, i64} %pair, 0
  %v2 = extractvalue {i64, i64} %pair, 1
  %res = call {i64, i64}* @malloc(i64 16)
  %first = getelementptr {i64, i64}, ptr %res, i64 0, i32 0
  %second = getelementptr {i64, i64}, ptr %res, i64 0, i32 1
  %v1.new = call i64 @callExtended(i64 %v1.old, ptr %fp)
  store i64 %v1.new, i64* %first
  store i64 %v2, i64* %second
  ret ptr %res
}

define ptr @second(i64 %arg) {
  %res = call {i64, i64, ptr}* @malloc(i64 24)
  %zero = getelementptr {i64, i64, ptr}, ptr %res, i64 0, i32 0
  %arg.extended = getelementptr {i64, i64, ptr}, ptr %res, i64 0, i32 1
  %fp.extended = getelementptr {i64, i64, ptr}, ptr %res, i64 0, i32 2
  store i64 0, i64* %zero
  store i64 %arg, i64* %arg.extended
  store ptr @secondInner, ptr %fp.extended
  ret ptr %res
}

define ptr @secondInner(ptr %arg, ptr %fp) {
  %pair = load {i64, i64}, ptr %arg
  %v1 = extractvalue {i64, i64} %pair, 0
  %v2.old = extractvalue {i64, i64} %pair, 1
  %res = call {i64, i64}* @malloc(i64 16)
  %first = getelementptr {i64, i64}, ptr %res, i64 0, i32 0
  %second = getelementptr {i64, i64}, ptr %res, i64 0, i32 1
  %v2.new = call i64 @callExtended(i64 %v2.old, ptr %fp)
  store i64 %v1, i64* %first
  store i64 %v2.new, i64* %second
  ret ptr %res
}

define ptr @tripleAsterisk(i64 %arg) {
  %res = call {i64, i64, ptr}* @malloc(i64 24)
  %zero = getelementptr {i64, i64, ptr}, ptr %res, i64 0, i32 0
  %arg.extended = getelementptr {i64, i64, ptr}, ptr %res, i64 0, i32 1
  %fp.extended = getelementptr {i64, i64, ptr}, ptr %res, i64 0, i32 2
  store i64 0, i64* %zero
  store i64 %arg, i64* %arg.extended
  store ptr @tripleAsteriskInner, ptr %fp.extended
  ret ptr %res
}

define ptr @tripleAsteriskInner(ptr %arg, ptr %fps) {
  %fps.pair = load {ptr, ptr}, ptr %fps
  %fp1 = extractvalue {ptr, ptr} %fps.pair, 0
  %fp2 = extractvalue {ptr, ptr} %fps.pair, 1
  %arg.pair = load {i64, i64}, ptr %arg
  %v1.old = extractvalue {i64, i64} %arg.pair, 0
  %v2.old = extractvalue {i64, i64} %arg.pair, 1
  %res = call {i64, i64}* @malloc(i64 16)
  %first = getelementptr {i64, i64}, ptr %res, i64 0, i32 0
  %second = getelementptr {i64, i64}, ptr %res, i64 0, i32 1
  %v1.new = call i64 @callExtended(i64 %v1.old, ptr %fp1)
  %v2.new = call i64 @callExtended(i64 %v2.old, ptr %fp2)
  store i64 %v1.new, i64* %first
  store i64 %v2.new, i64* %second
  ret ptr %res
}

define ptr @tripleAnd(i64 %arg) {
  %res = call {i64, i64, ptr}* @malloc(i64 24)
  %zero = getelementptr {i64, i64, ptr}, ptr %res, i64 0, i32 0
  %arg.extended = getelementptr {i64, i64, ptr}, ptr %res, i64 0, i32 1
  %fp.extended = getelementptr {i64, i64, ptr}, ptr %res, i64 0, i32 2
  store i64 0, i64* %zero
  store i64 %arg, i64* %arg.extended
  store ptr @tripleAndInner, ptr %fp.extended
  ret ptr %res
}

define ptr @tripleAndInner(i64 %arg, ptr %fps) {
  %pair = load {ptr, ptr}, ptr %fps
  %fp1 = extractvalue {ptr, ptr} %pair, 0
  %fp2 = extractvalue {ptr, ptr} %pair, 1
  %res = call {i64, i64}* @malloc(i64 16)
  %first = getelementptr {i64, i64}, ptr %res, i64 0, i32 0
  %second = getelementptr {i64, i64}, ptr %res, i64 0, i32 1
  %v1 = call i64 @callExtended(i64 %arg, ptr %fp1)
  %v2 = call i64 @callExtended(i64 %arg, ptr %fp2)
  store i64 %v1, i64* %first
  store i64 %v2, i64* %second
  ret ptr %res
}

define ptr @left(i64 %arg) {
  %res = call {i64, i64, ptr}* @malloc(i64 24)
  %zero = getelementptr {i64, i64, ptr}, ptr %res, i64 0, i32 0
  %arg.extended = getelementptr {i64, i64, ptr}, ptr %res, i64 0, i32 1
  %fp.extended = getelementptr {i64, i64, ptr}, ptr %res, i64 0, i32 2
  store i64 0, i64* %zero
  store i64 %arg, i64* %arg.extended
  store ptr @leftInner, ptr %fp.extended
  ret ptr %res
}

define ptr @leftInner(ptr %arg, ptr %fp) {
  %pair = load {i64, i64}, ptr %arg
  %choice = extractvalue {i64, i64} %pair, 0
  %v.old = extractvalue {i64, i64} %pair, 1
  %choice.bool = icmp eq i64 %choice, 0
  br i1 %choice.bool, label %left, label %done
left:
  %v.new = call i64 @callExtended(i64 %v.old, ptr %fp)
  %res = call {i64, i64}* @malloc(i64 16)
  %first = getelementptr {i64, i64}, ptr %res, i64 0, i32 0
  %second = getelementptr {i64, i64}, ptr %res, i64 0, i32 1
  store i64 %choice, i64* %first
  store i64 %v.new, i64* %second
  ret ptr %res
done:
  ret ptr %arg
}

define ptr @right(i64 %arg) {
  %res = call {i64, i64, ptr}* @malloc(i64 24)
  %zero = getelementptr {i64, i64, ptr}, ptr %res, i64 0, i32 0
  %arg.extended = getelementptr {i64, i64, ptr}, ptr %res, i64 0, i32 1
  %fp.extended = getelementptr {i64, i64, ptr}, ptr %res, i64 0, i32 2
  store i64 0, i64* %zero
  store i64 %arg, i64* %arg.extended
  store ptr @rightInner, ptr %fp.extended
  ret ptr %res
}

define ptr @rightInner(ptr %arg, ptr %fp) {
  %pair = load {i64, i64}, ptr %arg
  %choice = extractvalue {i64, i64} %pair, 0
  %v.old = extractvalue {i64, i64} %pair, 1
  %choice.bool = icmp eq i64 %choice, 0
  br i1 %choice.bool, label %done, label %right
right:
  %v.new = call i64 @callExtended(i64 %v.old, ptr %fp)
  %res = call {i64, i64}* @malloc(i64 16)
  %first = getelementptr {i64, i64}, ptr %res, i64 0, i32 0
  %second = getelementptr {i64, i64}, ptr %res, i64 0, i32 1
  store i64 %choice, i64* %first
  store i64 %v.new, i64* %second
  ret ptr %res
done:
  ret ptr %arg
}

define ptr @triplePlus(i64 %arg) {
  %res = call {i64, i64, ptr}* @malloc(i64 24)
  %zero = getelementptr {i64, i64, ptr}, ptr %res, i64 0, i32 0
  %arg.extended = getelementptr {i64, i64, ptr}, ptr %res, i64 0, i32 1
  %fp.extended = getelementptr {i64, i64, ptr}, ptr %res, i64 0, i32 2
  store i64 0, i64* %zero
  store i64 %arg, i64* %arg.extended
  store ptr @triplePlusInner, ptr %fp.extended
  ret ptr %res
}

define ptr @triplePlusInner(ptr %arg, ptr %fps) {
  %fps.pair = load {ptr, ptr}, ptr %fps
  %fp1 = extractvalue {ptr, ptr} %fps.pair, 0
  %fp2 = extractvalue {ptr, ptr} %fps.pair, 1
  %arg.pair = load {i64, i64}, ptr %arg
  %choice = extractvalue {i64, i64} %arg.pair, 0
  %v.old = extractvalue {i64, i64} %arg.pair, 1
  %res = call {i64, i64}* @malloc(i64 16)
  %first = getelementptr {i64, i64}, ptr %res, i64 0, i32 0
  %second = getelementptr {i64, i64}, ptr %res, i64 0, i32 1
  store i64 %choice, i64* %first
  %choice.bool = icmp eq i64 %choice, 0
  br i1 %choice.bool, label %left, label %right
left:
  %v.left.new = call i64 @callExtended(i64 %v.old, ptr %fp1)
  store i64 %v.left.new, i64* %second
  ret ptr %res
right:
  %v.right.new = call i64 @callExtended(i64 %v.old, ptr %fp2)
  store i64 %v.right.new, i64* %second
  ret ptr %res
}

define ptr @tripleBar(i64 %arg) {
  %res = call {i64, i64, ptr}* @malloc(i64 24)
  %zero = getelementptr {i64, i64, ptr}, ptr %res, i64 0, i32 0
  %arg.extended = getelementptr {i64, i64, ptr}, ptr %res, i64 0, i32 1
  %fp.extended = getelementptr {i64, i64, ptr}, ptr %res, i64 0, i32 2
  store i64 0, i64* %zero
  store i64 %arg, i64* %arg.extended
  store ptr @tripleBarInner, ptr %fp.extended
  ret ptr %res
}

define i64 @tripleBarInner(ptr %arg, ptr %fps) {
  %fps.pair = load {ptr, ptr}, ptr %fps
  %fp1 = extractvalue {ptr, ptr} %fps.pair, 0
  %fp2 = extractvalue {ptr, ptr} %fps.pair, 1
  %arg.pair = load {i64, i64}, ptr %arg
  %choice = extractvalue {i64, i64} %arg.pair, 0
  %v.old = extractvalue {i64, i64} %arg.pair, 1
  %choice.bool = icmp eq i64 %choice, 0
  br i1 %choice.bool, label %left, label %right
left:
  %v.left.new = call i64 @callExtended(i64 %v.old, ptr %fp1)
  ret i64 %v.left.new
right:
  %v.right.new = call i64 @callExtended(i64 %v.old, ptr %fp2)
  ret i64 %v.right.new
}

define i64 @callExtended(i64 %arg, ptr %fp) {
  %fp.inner = load i64, ptr %fp
  %zero = icmp eq i64 %fp.inner, 0
  br i1 %zero, label %extended, label %normal
extended:
  %inner = load {i64, i64, ptr}, ptr %fp
  %second = extractvalue {i64, i64, ptr} %inner, 1
  %fp.actual = extractvalue {i64, i64, ptr} %inner, 2
  %res.extended = call i64 %fp.actual(i64 %arg, i64 %second)
  ret i64 %res.extended
normal:
  %res.normal = call i64 %fp(i64 %arg)
  ret i64 %res.normal
}
