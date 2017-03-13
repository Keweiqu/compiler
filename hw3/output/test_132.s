	.text
	.globl	foo
foo:
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
	addq	$-88, %rsp
	movq	%rsp, -64(%rbp)
	addq	$8, %rsp
	movq	-56(%rbp), %rcx
	movq	-48(%rbp), %r11
	addq	%rcx, %r11
	movq	%r11, -72(%rbp)
	movq	-72(%rbp), %r11
	movq	-64(%rbp), %rcx
	movq	%r11, (%rcx)
	movq	-64(%rbp), %rcx
	movq	(%rcx), %r11
	movq	%r11, -80(%rbp)
	movq	-40(%rbp), %r15
	movq	-32(%rbp), %r14
	movq	-24(%rbp), %r13
	movq	-16(%rbp), %r12
	movq	-8(%rbp), %rbx
	movq	-80(%rbp), %rax
	movq	%rbp, %rsp
	addq	$8, %rsp
	movq	(%rbp), %rbp
	retq	
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
	addq	$-80, %rsp
	pushq	%r11
	pushq	%r10
	pushq	%r9
	pushq	%r8
	pushq	%rdi
	pushq	%rsi
	pushq	%rdx
	pushq	%rcx
	movq	$foo, %rdi
	callq	ll_callback
	addq	$24, %rsp
	movq	-8(%rsp), %r11
	movq	-16(%rsp), %r10
	movq	-24(%rsp), %r9
	movq	-32(%rsp), %r8
	movq	-40(%rsp), %rdi
	movq	-48(%rsp), %rsi
	movq	-56(%rsp), %rdx
	movq	-64(%rsp), %rcx
	movq	%rax, -64(%rbp)
	pushq	%r11
	pushq	%r10
	pushq	%r9
	pushq	%r8
	pushq	%rdi
	pushq	%rsi
	pushq	%rdx
	pushq	%rcx
	movq	-64(%rbp), %rdi
	callq	ll_ltoa
	addq	$24, %rsp
	movq	-8(%rsp), %r11
	movq	-16(%rsp), %r10
	movq	-24(%rsp), %r9
	movq	-32(%rsp), %r8
	movq	-40(%rsp), %rdi
	movq	-48(%rsp), %rsi
	movq	-56(%rsp), %rdx
	movq	-64(%rsp), %rcx
	movq	%rax, -72(%rbp)
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