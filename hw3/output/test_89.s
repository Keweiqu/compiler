	.data
	.globl	toofew
toofew:
	.asciz	"argc < 3"
	.data
	.globl	toomany
toomany:
	.asciz	"argc > 3"
	.text
	.globl	main
main:
	movq	%rsp, %rbp
	pushq	%rbp
	movq	%rsp, %rbp
	pushq	%rbx
	pushq	%r12
	pushq	%r13
	pushq	%r14
	pushq	%r15
	pushq	%rdi
	pushq	%rsi
	movq	%rbp, %rsp
	addq	$-136, %rsp
	movq	$0, -64(%rbp)
	movq	-48(%rbp), %r11
	cmpq	$3, %r11
	setl	-64(%rbp)
	cmpq	$0, -64(%rbp)
	je	else
	jne	few
	.text
few:
	movq	$toofew, %r14
	movq	$0, %r10
	movq	$0, %r11
	imulq	%r10, %r11
	addq	%r11, %r14
	movq	$0, %r10
	movq	$0, %r11
	imulq	%r10, %r11
	addq	%r11, %r14
	movq	%r14, -72(%rbp)
	pushq	%r11
	pushq	%r10
	pushq	%r9
	pushq	%r8
	pushq	%rdi
	pushq	%rsi
	pushq	%rdx
	pushq	%rcx
	movq	-72(%rbp), %rdi
	callq	ll_puts
	addq	$24, %rsp
	movq	-8(%rsp), %r11
	movq	-16(%rsp), %r10
	movq	-24(%rsp), %r9
	movq	-32(%rsp), %r8
	movq	-40(%rsp), %rdi
	movq	-48(%rsp), %rsi
	movq	-56(%rsp), %rdx
	movq	-64(%rsp), %rcx
	movq	-40(%rbp), %r15
	movq	-32(%rbp), %r14
	movq	-24(%rbp), %r13
	movq	-16(%rbp), %r12
	movq	-8(%rbp), %rbx
	movq	$0, %rax
	movq	%rbp, %rsp
	addq	$8, %rsp
	movq	(%rbp), %rbp
	retq	
	.text
else:
	movq	$0, -80(%rbp)
	movq	-48(%rbp), %r11
	cmpq	$3, %r11
	setg	-80(%rbp)
	cmpq	$0, -80(%rbp)
	je	right
	jne	many
	.text
many:
	movq	$toomany, %r14
	movq	$0, %r10
	movq	$0, %r11
	imulq	%r10, %r11
	addq	%r11, %r14
	movq	$0, %r10
	movq	$0, %r11
	imulq	%r10, %r11
	addq	%r11, %r14
	movq	%r14, -88(%rbp)
	pushq	%r11
	pushq	%r10
	pushq	%r9
	pushq	%r8
	pushq	%rdi
	pushq	%rsi
	pushq	%rdx
	pushq	%rcx
	movq	-88(%rbp), %rdi
	callq	ll_puts
	addq	$24, %rsp
	movq	-8(%rsp), %r11
	movq	-16(%rsp), %r10
	movq	-24(%rsp), %r9
	movq	-32(%rsp), %r8
	movq	-40(%rsp), %rdi
	movq	-48(%rsp), %rsi
	movq	-56(%rsp), %rdx
	movq	-64(%rsp), %rcx
	movq	-40(%rbp), %r15
	movq	-32(%rbp), %r14
	movq	-24(%rbp), %r13
	movq	-16(%rbp), %r12
	movq	-8(%rbp), %rbx
	movq	$0, %rax
	movq	%rbp, %rsp
	addq	$8, %rsp
	movq	(%rbp), %rbp
	retq	
	.text
right:
	movq	-56(%rbp), %r14
	movq	$8, %r10
	movq	$1, %r11
	imulq	%r10, %r11
	addq	%r11, %r14
	movq	%r14, -96(%rbp)
	movq	-96(%rbp), %rcx
	movq	(%rcx), %r11
	movq	%r11, -104(%rbp)
	movq	-56(%rbp), %r14
	movq	$8, %r10
	movq	$2, %r11
	imulq	%r10, %r11
	addq	%r11, %r14
	movq	%r14, -112(%rbp)
	movq	-112(%rbp), %rcx
	movq	(%rcx), %r11
	movq	%r11, -120(%rbp)
	pushq	%r11
	pushq	%r10
	pushq	%r9
	pushq	%r8
	pushq	%rdi
	pushq	%rsi
	pushq	%rdx
	pushq	%rcx
	movq	-104(%rbp), %rdi
	movq	-120(%rbp), %rsi
	callq	ll_strcat
	addq	$32, %rsp
	movq	-8(%rsp), %r11
	movq	-16(%rsp), %r10
	movq	-24(%rsp), %r9
	movq	-32(%rsp), %r8
	movq	-40(%rsp), %rdi
	movq	-48(%rsp), %rsi
	movq	-56(%rsp), %rdx
	movq	-64(%rsp), %rcx
	movq	%rax, -128(%rbp)
	pushq	%r11
	pushq	%r10
	pushq	%r9
	pushq	%r8
	pushq	%rdi
	pushq	%rsi
	pushq	%rdx
	pushq	%rcx
	movq	-128(%rbp), %rdi
	callq	ll_puts
	addq	$24, %rsp
	movq	-8(%rsp), %r11
	movq	-16(%rsp), %r10
	movq	-24(%rsp), %r9
	movq	-32(%rsp), %r8
	movq	-40(%rsp), %rdi
	movq	-48(%rsp), %rsi
	movq	-56(%rsp), %rdx
	movq	-64(%rsp), %rcx
	movq	-40(%rbp), %r15
	movq	-32(%rbp), %r14
	movq	-24(%rbp), %r13
	movq	-16(%rbp), %r12
	movq	-8(%rbp), %rbx
	movq	$0, %rax
	movq	%rbp, %rsp
	addq	$8, %rsp
	movq	(%rbp), %rbp
	retq	