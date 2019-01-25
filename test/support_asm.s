.bits 64

; void putc(int c);

.global _putc
_putc:		push rbp
		mov rbp, rsp
		push r11
		push rdi
		push rsi

		mov rax, 1		; sys_write
		mov edi, 1		; fd 1 = stdout
		lea rsi, qword [rbp,16]	; &c
		mov edx, 1		; length 1
		syscall

		pop rsi
		pop rdi
		pop r11
		pop rbp
		ret

; __blkcpy (compiler support)
; RAX = src
; RDX = dst
; RCX = count

.global ___blkcpy
___blkcpy:	push rsi
		push rdi

		mov rdi, rdx
		mov rsi, rax
		cld
		rep
		movsb

		pop rdi
		pop rsi
		ret

