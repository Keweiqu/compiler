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
	movq	%rbp, %rsp
	addq	$-48, %rsp
	movq	-40(%rbp), %r15
	movq	-32(%rbp), %r14
	movq	-24(%rbp), %r13
	movq	-16(%rbp), %r12
	movq	-8(%rbp), %rbx
	movq	$42, %rax
	movq	%rbp, %rsp
	addq	$8, %rsp
	movq	(%rbp), %rbp
	retq	
	.text
	.globl	bar
bar:
	pushq	%rbp
	movq	%rsp, %rbp
	pushq	%rbx
	pushq	%r12
	pushq	%r13
	pushq	%r14
	pushq	%r15
	movq	%rbp, %rsp
	addq	$-48, %rsp
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
	addq	$-120, %rsp
	movq	%rsp, -64(%rbp)
	addq	$8, %rsp
	movq	%rsp, -72(%rbp)
	addq	$8, %rsp
	movq	$0, %r11
	movq	-64(%rbp), %rcx
	movq	%r11, (%rcx)
	movq	$100, %r11
	movq	-72(%rbp), %rcx
	movq	%r11, (%rcx)
	movq	-72(%rbp), %rcx
	movq	(%rcx), %r11
	movq	%r11, -80(%rbp)
	movq	$0, -88(%rbp)
	movq	-80(%rbp), %r11
	cmpq	$0, %r11
	setne	-88(%rbp)
	cmpq	$0, -88(%rbp)
	je	else
	jne	then
	.text
then:
	pushq	%r11
	pushq	%r10
	pushq	%r9
	pushq	%r8
	pushq	%rdi
	pushq	%rsi
	pushq	%rdx
	pushq	%rcx
	callq	foo
	addq	$16, %rsp
	movq	-8(%rsp), %r11
	movq	-16(%rsp), %r10
	movq	-24(%rsp), %r9
	movq	-32(%rsp), %r8
	movq	-40(%rsp), %rdi
	movq	-48(%rsp), %rsi
	movq	-56(%rsp), %rdx
	movq	-64(%rsp), %rcx
	movq	%rax, -96(%rbp)
	movq	-96(%rbp), %r11
	movq	-64(%rbp), %rcx
	movq	%r11, (%rcx)
	jmp	end
	.text
else:
	pushq	%r11
	pushq	%r10
	pushq	%r9
	pushq	%r8
	pushq	%rdi
	pushq	%rsi
	pushq	%rdx
	pushq	%rcx
	callq	bar
	addq	$16, %rsp
	movq	-8(%rsp), %r11
	movq	-16(%rsp), %r10
	movq	-24(%rsp), %r9
	movq	-32(%rsp), %r8
	movq	-40(%rsp), %rdi
	movq	-48(%rsp), %rsi
	movq	-56(%rsp), %rdx
	movq	-64(%rsp), %rcx
	movq	%rax, -104(%rbp)
	movq	-104(%rbp), %r11
	movq	-64(%rbp), %rcx
	movq	%r11, (%rcx)
	jmp	end
	.text
end:
	movq	-64(%rbp), %rcx
	movq	(%rcx), %r11
	movq	%r11, -112(%rbp)
	movq	-40(%rbp), %r15
	movq	-32(%rbp), %r14
	movq	-24(%rbp), %r13
	movq	-16(%rbp), %r12
	movq	-8(%rbp), %rbx
	movq	-112(%rbp), %rax
	movq	%rbp, %rsp
	addq	$8, %rsp
	movq	(%rbp), %rbp
	retq	