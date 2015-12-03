	.cstring
LC0:
	.ascii "go.stdlib#symbol\0"
	.text
	.align 4,0x90
.globl _initSymbolClass
_initSymbolClass:
	pushl	%ebp
	movl	%esp, %ebp
	pushl	%ebx
	call	L3
"L00000000001$pb":
L3:
	popl	%ebx
	subl	$36, %esp
	leal	_syHashFun-"L00000000001$pb"(%ebx), %eax
	movl	%eax, 24(%esp)
	leal	_syScanFun-"L00000000001$pb"(%ebx), %eax
	movl	%eax, 20(%esp)
	leal	_syCopyFun-"L00000000001$pb"(%ebx), %eax
	movl	%eax, 16(%esp)
	leal	_syOutFun-"L00000000001$pb"(%ebx), %eax
	movl	%eax, 12(%esp)
	leal	_syCompFun-"L00000000001$pb"(%ebx), %eax
	movl	%eax, 8(%esp)
	leal	_sySizeFun-"L00000000001$pb"(%ebx), %eax
	movl	%eax, 4(%esp)
	leal	LC0-"L00000000001$pb"(%ebx), %eax
	movl	%eax, (%esp)
	call	L_newSpecialClass$stub
	movl	L_symbolClass$non_lazy_ptr-"L00000000001$pb"(%ebx), %edx
	movl	%eax, (%edx)
	addl	$36, %esp
	popl	%ebx
	leave
	ret
	.align 4,0x90
_sySizeFun:
	pushl	%ebp
	movl	%esp, %ebp
	subl	$24, %esp
	movl	12(%ebp), %eax
	addl	$4, %eax
	movl	%eax, (%esp)
	call	L_uniStrLen$stub
	leave
	leal	9(%eax,%eax), %eax
	shrl	$2, %eax
	ret
	.align 4,0x90
_syScanFun:
	pushl	%ebp
	xorl	%eax, %eax
	movl	%esp, %ebp
	leave
	ret
	.align 4,0x90
.globl _permUniSymbol
_permUniSymbol:
	pushl	%ebp
	movl	%esp, %ebp
	subl	$40, %esp
	movl	%ebx, -12(%ebp)
	movl	8(%ebp), %eax
	call	L10
"L00000000002$pb":
L10:
	popl	%ebx
	movl	%esi, -8(%ebp)
	movl	%edi, -4(%ebp)
	movl	%eax, (%esp)
	call	L_uniStrLen$stub
	leal	(%eax,%eax), %edi
	leal	9(%edi), %eax
	shrl	$2, %eax
	movl	%eax, (%esp)
	call	L_permAllocate$stub
	leal	2(%edi), %edx
	movl	%eax, %esi
	movl	L_symbolClass$non_lazy_ptr-"L00000000002$pb"(%ebx), %eax
	movl	(%eax), %eax
	movl	%eax, (%esi)
	leal	4(%esi), %eax
	orl	$1, %esi
	movl	%edx, 8(%esp)
	movl	8(%ebp), %edx
	movl	%eax, (%esp)
	movl	%edx, 4(%esp)
	call	L_memcpy$stub
	movl	%esi, %eax
	movl	-12(%ebp), %ebx
	movl	-8(%ebp), %esi
	movl	-4(%ebp), %edi
	leave
	ret
	.align 4,0x90
.globl _symbolPresent
_symbolPresent:
	pushl	%ebp
	movl	%esp, %ebp
	pushl	%ebx
	call	L13
"L00000000003$pb":
L13:
	popl	%ebx
	subl	$20, %esp
	movl	_dictionary-"L00000000003$pb"(%ebx), %eax
	movl	%eax, 4(%esp)
	movl	8(%ebp), %eax
	movl	%eax, (%esp)
	call	L_Search$stub
	addl	$20, %esp
	popl	%ebx
	leave
	ret
	.align 4,0x90
_remSym:
	pushl	%ebp
	movl	%esp, %ebp
	subl	$24, %esp
	movl	%ebx, -8(%ebp)
	call	L21
"L00000000004$pb":
L21:
	popl	%ebx
	movl	%esi, -4(%ebp)
	movl	12(%ebp), %esi
	movl	_dictionary-"L00000000004$pb"(%ebx), %eax
	movl	%eax, 4(%esp)
	movl	8(%ebp), %eax
	movl	%eax, (%esp)
	call	L_Uninstall$stub
	movl	L_globalSpace$non_lazy_ptr-"L00000000004$pb"(%ebx), %eax
	movl	%esi, %edx
	andl	$-4, %edx
	cmpl	(%eax), %edx
	jb	L15
	movl	L_leftBase$non_lazy_ptr-"L00000000004$pb"(%ebx), %eax
	cmpl	(%eax), %edx
	jb	L20
L15:
	movl	-8(%ebp), %ebx
	xorl	%eax, %eax
	movl	-4(%ebp), %esi
	leave
	ret
	.align 4,0x90
L20:
	movl	16(%ebp), %ecx
	movl	4(%ecx), %eax
	movl	%esi, 4(%esp)
	movl	%eax, 8(%esp)
	leal	4(%edx), %eax
	movl	%eax, (%esp)
	call	L_Install$stub
	movl	-8(%ebp), %ebx
	xorl	%eax, %eax
	movl	-4(%ebp), %esi
	leave
	ret
	.align 4,0x90
.globl _restartDictionary
_restartDictionary:
	pushl	%ebp
	movl	%esp, %ebp
	pushl	%edi
	pushl	%esi
	pushl	%ebx
	call	L28
"L00000000005$pb":
L28:
	popl	%ebx
	subl	$44, %esp
	movl	$0, 12(%esp)
	movl	$256, (%esp)
	movl	L_uniCmp$non_lazy_ptr-"L00000000005$pb"(%ebx), %eax
	movl	%eax, 8(%esp)
	movl	L_uniHash$non_lazy_ptr-"L00000000005$pb"(%ebx), %eax
	movl	%eax, 4(%esp)
	call	L_NewHash$stub
	movl	8(%ebp), %edx
	movl	_dictionary-"L00000000005$pb"(%ebx), %esi
	movl	%edx, -32(%ebp)
	movl	%esi, 4(%esp)
	movl	%eax, -28(%ebp)
	movl	%eax, _dictionary-"L00000000005$pb"(%ebx)
	leal	-32(%ebp), %eax
	movl	%eax, 8(%esp)
	leal	_remSym-"L00000000005$pb"(%ebx), %eax
	movl	%eax, (%esp)
	call	L_ProcessTable$stub
	movl	%esi, (%esp)
	call	L_DelHash$stub
	movl	8(%ebp), %eax
	movl	%eax, (%esp)
	call	L_markStandardClasses$stub
	movl	8(%ebp), %edx
	movl	%edx, (%esp)
	call	L_restartChars$stub
	movl	8(%ebp), %eax
	movl	%eax, (%esp)
	call	L_markPrograms$stub
	movl	L_kmain$non_lazy_ptr-"L00000000005$pb"(%ebx), %esi
	movl	8(%ebp), %edx
	movl	(%esi), %eax
	movl	%edx, (%esp)
	movl	%eax, 4(%esp)
	call	L_scanPtr$stub
	movl	%eax, (%esi)
	movl	L_kmainThread$non_lazy_ptr-"L00000000005$pb"(%ebx), %esi
	movl	(%esi), %eax
	movl	%eax, 4(%esp)
	movl	8(%ebp), %eax
	movl	%eax, (%esp)
	call	L_scanPtr$stub
	movl	8(%ebp), %edx
	movl	%eax, (%esi)
	movl	L_bootProg$non_lazy_ptr-"L00000000005$pb"(%ebx), %esi
	movl	(%esi), %eax
	movl	%edx, (%esp)
	movl	%eax, 4(%esp)
	call	L_scanPtr$stub
	movl	%eax, (%esi)
	movl	L_dieProg$non_lazy_ptr-"L00000000005$pb"(%ebx), %esi
	movl	(%esi), %eax
	movl	%eax, 4(%esp)
	movl	8(%ebp), %eax
	movl	%eax, (%esp)
	call	L_scanPtr$stub
	movl	8(%ebp), %edx
	movl	%eax, (%esi)
	movl	L_exitProg$non_lazy_ptr-"L00000000005$pb"(%ebx), %esi
	movl	(%esi), %eax
	movl	%edx, (%esp)
	movl	%eax, 4(%esp)
	call	L_scanPtr$stub
	movl	%eax, (%esi)
	movl	L_trapProg$non_lazy_ptr-"L00000000005$pb"(%ebx), %esi
	movl	(%esi), %eax
	movl	%eax, 4(%esp)
	movl	8(%ebp), %eax
	movl	%eax, (%esp)
	call	L_scanPtr$stub
	movl	8(%ebp), %edx
	movl	%eax, (%esi)
	movl	L_kprocessFlag$non_lazy_ptr-"L00000000005$pb"(%ebx), %esi
	movl	(%esi), %eax
	movl	%edx, (%esp)
	movl	%eax, 4(%esp)
	call	L_scanPtr$stub
	movl	%eax, (%esi)
	movl	L_kvoid$non_lazy_ptr-"L00000000005$pb"(%ebx), %esi
	movl	(%esi), %eax
	movl	%eax, 4(%esp)
	movl	8(%ebp), %eax
	movl	%eax, (%esp)
	call	L_scanPtr$stub
	movl	8(%ebp), %edx
	movl	%eax, (%esi)
	movl	L_emptySymbol$non_lazy_ptr-"L00000000005$pb"(%ebx), %esi
	movl	(%esi), %eax
	movl	%edx, (%esp)
	movl	%eax, 4(%esp)
	call	L_scanPtr$stub
	movl	%eax, (%esi)
	movl	L_zero$non_lazy_ptr-"L00000000005$pb"(%ebx), %esi
	movl	(%esi), %eax
	movl	%eax, 4(%esp)
	movl	8(%ebp), %eax
	movl	%eax, (%esp)
	call	L_scanPtr$stub
	movl	8(%ebp), %edx
	movl	%eax, (%esi)
	movl	L_kfifo$non_lazy_ptr-"L00000000005$pb"(%ebx), %esi
	movl	(%esi), %eax
	movl	%edx, (%esp)
	movl	%eax, 4(%esp)
	call	L_scanPtr$stub
	movl	%eax, (%esi)
	movl	L_kdir$non_lazy_ptr-"L00000000005$pb"(%ebx), %esi
	movl	(%esi), %eax
	movl	%eax, 4(%esp)
	movl	8(%ebp), %eax
	movl	%eax, (%esp)
	call	L_scanPtr$stub
	movl	8(%ebp), %edx
	movl	%eax, (%esi)
	movl	L_kcharfile$non_lazy_ptr-"L00000000005$pb"(%ebx), %esi
	movl	(%esi), %eax
	movl	%edx, (%esp)
	movl	%eax, 4(%esp)
	call	L_scanPtr$stub
	movl	%eax, (%esi)
	movl	L_kblock$non_lazy_ptr-"L00000000005$pb"(%ebx), %esi
	movl	(%esi), %eax
	movl	%eax, 4(%esp)
	movl	8(%ebp), %eax
	movl	%eax, (%esp)
	call	L_scanPtr$stub
	movl	8(%ebp), %edx
	movl	%eax, (%esi)
	movl	L_kplain$non_lazy_ptr-"L00000000005$pb"(%ebx), %esi
	movl	(%esi), %eax
	movl	%edx, (%esp)
	movl	%eax, 4(%esp)
	call	L_scanPtr$stub
	movl	%eax, (%esi)
	movl	L_ksymlink$non_lazy_ptr-"L00000000005$pb"(%ebx), %esi
	movl	(%esi), %eax
	movl	%eax, 4(%esp)
	movl	8(%ebp), %eax
	movl	%eax, (%esp)
	call	L_scanPtr$stub
	movl	8(%ebp), %edx
	movl	%eax, (%esi)
	movl	L_ksock$non_lazy_ptr-"L00000000005$pb"(%ebx), %esi
	movl	(%esi), %eax
	movl	%edx, (%esp)
	movl	%eax, 4(%esp)
	call	L_scanPtr$stub
	movl	%eax, (%esi)
	movl	L_kunknown$non_lazy_ptr-"L00000000005$pb"(%ebx), %esi
	movl	(%esi), %eax
	movl	%eax, 4(%esp)
	movl	8(%ebp), %eax
	movl	%eax, (%esp)
	call	L_scanPtr$stub
	movl	8(%ebp), %edx
	movl	%eax, (%esi)
	movl	L_kloadflag$non_lazy_ptr-"L00000000005$pb"(%ebx), %esi
	movl	(%esi), %eax
	movl	%edx, (%esp)
	movl	%eax, 4(%esp)
	call	L_scanPtr$stub
	movl	%eax, (%esi)
	movl	L_kversion$non_lazy_ptr-"L00000000005$pb"(%ebx), %esi
	movl	(%esi), %eax
	movl	%eax, 4(%esp)
	movl	8(%ebp), %eax
	movl	%eax, (%esp)
	call	L_scanPtr$stub
	movl	8(%ebp), %edx
	movl	%eax, (%esi)
	movl	L_universal$non_lazy_ptr-"L00000000005$pb"(%ebx), %esi
	movl	(%esi), %eax
	movl	%edx, (%esp)
	movl	%eax, 4(%esp)
	call	L_scanPtr$stub
	movl	%eax, (%esi)
	movl	L_kdefined$non_lazy_ptr-"L00000000005$pb"(%ebx), %esi
	movl	(%esi), %eax
	movl	%eax, 4(%esp)
	movl	8(%ebp), %eax
	movl	%eax, (%esp)
	call	L_scanPtr$stub
	movl	8(%ebp), %edx
	movl	%eax, (%esi)
	movl	L_klabel$non_lazy_ptr-"L00000000005$pb"(%ebx), %esi
	movl	(%esi), %eax
	movl	%edx, (%esp)
	movl	%eax, 4(%esp)
	call	L_scanPtr$stub
	movl	%eax, (%esi)
	movl	L_kstart$non_lazy_ptr-"L00000000005$pb"(%ebx), %esi
	movl	(%esi), %eax
	movl	%eax, 4(%esp)
	movl	8(%ebp), %eax
	movl	%eax, (%esp)
	call	L_scanPtr$stub
	movl	%eax, (%esi)
	movl	L_doResume$non_lazy_ptr-"L00000000005$pb"(%ebx), %esi
	leal	256(%esi), %edi
	.align 4,0x90
L23:
	movl	(%esi), %eax
	movl	8(%ebp), %edx
	movl	%eax, 4(%esp)
	movl	%edx, (%esp)
	call	L_scanPtr$stub
	movl	%eax, (%esi)
	addl	$4, %esi
	cmpl	%esi, %edi
	jne	L23
	movl	L_kdelay$non_lazy_ptr-"L00000000005$pb"(%ebx), %esi
	movl	(%esi), %eax
	movl	%eax, 4(%esp)
	movl	8(%ebp), %eax
	movl	%eax, (%esp)
	call	L_scanPtr$stub
	movl	8(%ebp), %edx
	movl	%eax, (%esi)
	movl	L_eINSUFARG$non_lazy_ptr-"L00000000005$pb"(%ebx), %esi
	movl	(%esi), %eax
	movl	%edx, (%esp)
	movl	%eax, 4(%esp)
	call	L_scanPtr$stub
	movl	%eax, (%esi)
	movl	L_eVARNEEDD$non_lazy_ptr-"L00000000005$pb"(%ebx), %esi
	movl	(%esi), %eax
	movl	%eax, 4(%esp)
	movl	8(%ebp), %eax
	movl	%eax, (%esp)
	call	L_scanPtr$stub
	movl	8(%ebp), %edx
	movl	%eax, (%esi)
	movl	L_eINTNEEDD$non_lazy_ptr-"L00000000005$pb"(%ebx), %esi
	movl	(%esi), %eax
	movl	%edx, (%esp)
	movl	%eax, 4(%esp)
	call	L_scanPtr$stub
	movl	%eax, (%esi)
	movl	L_eNUMNEEDD$non_lazy_ptr-"L00000000005$pb"(%ebx), %esi
	movl	(%esi), %eax
	movl	%eax, 4(%esp)
	movl	8(%ebp), %eax
	movl	%eax, (%esp)
	call	L_scanPtr$stub
	movl	8(%ebp), %edx
	movl	%eax, (%esi)
	movl	L_eSPACE$non_lazy_ptr-"L00000000005$pb"(%ebx), %esi
	movl	(%esi), %eax
	movl	%edx, (%esp)
	movl	%eax, 4(%esp)
	call	L_scanPtr$stub
	movl	%eax, (%esi)
	movl	L_eUNIFY$non_lazy_ptr-"L00000000005$pb"(%ebx), %esi
	movl	(%esi), %eax
	movl	%eax, 4(%esp)
	movl	8(%ebp), %eax
	movl	%eax, (%esp)
	call	L_scanPtr$stub
	movl	8(%ebp), %edx
	movl	%eax, (%esi)
	movl	L_eOCCUR$non_lazy_ptr-"L00000000005$pb"(%ebx), %esi
	movl	(%esi), %eax
	movl	%edx, (%esp)
	movl	%eax, 4(%esp)
	call	L_scanPtr$stub
	movl	%eax, (%esi)
	movl	L_eCODE$non_lazy_ptr-"L00000000005$pb"(%ebx), %esi
	movl	(%esi), %eax
	movl	%eax, 4(%esp)
	movl	8(%ebp), %eax
	movl	%eax, (%esp)
	call	L_scanPtr$stub
	movl	8(%ebp), %edx
	movl	%eax, (%esi)
	movl	L_eDIVZERO$non_lazy_ptr-"L00000000005$pb"(%ebx), %esi
	movl	(%esi), %eax
	movl	%edx, (%esp)
	movl	%eax, 4(%esp)
	call	L_scanPtr$stub
	movl	%eax, (%esi)
	movl	L_eLSTNEEDD$non_lazy_ptr-"L00000000005$pb"(%ebx), %esi
	movl	(%esi), %eax
	movl	%eax, 4(%esp)
	movl	8(%ebp), %eax
	movl	%eax, (%esp)
	call	L_scanPtr$stub
	movl	8(%ebp), %edx
	movl	%eax, (%esi)
	movl	L_eTPLNEEDD$non_lazy_ptr-"L00000000005$pb"(%ebx), %esi
	movl	(%esi), %eax
	movl	%edx, (%esp)
	movl	%eax, 4(%esp)
	call	L_scanPtr$stub
	movl	%eax, (%esi)
	movl	L_eSYMNEEDD$non_lazy_ptr-"L00000000005$pb"(%ebx), %esi
	movl	(%esi), %eax
	movl	%eax, 4(%esp)
	movl	8(%ebp), %eax
	movl	%eax, (%esp)
	call	L_scanPtr$stub
	movl	8(%ebp), %edx
	movl	%eax, (%esi)
	movl	L_eCHRNEEDD$non_lazy_ptr-"L00000000005$pb"(%ebx), %esi
	movl	(%esi), %eax
	movl	%edx, (%esp)
	movl	%eax, 4(%esp)
	call	L_scanPtr$stub
	movl	%eax, (%esi)
	movl	L_eSTRNEEDD$non_lazy_ptr-"L00000000005$pb"(%ebx), %esi
	movl	(%esi), %eax
	movl	%eax, 4(%esp)
	movl	8(%ebp), %eax
	movl	%eax, (%esp)
	call	L_scanPtr$stub
	movl	8(%ebp), %edx
	movl	%eax, (%esi)
	movl	L_eHANDLE$non_lazy_ptr-"L00000000005$pb"(%ebx), %esi
	movl	(%esi), %eax
	movl	%edx, (%esp)
	movl	%eax, 4(%esp)
	call	L_scanPtr$stub
	movl	%eax, (%esi)
	movl	L_eINVAL$non_lazy_ptr-"L00000000005$pb"(%ebx), %esi
	movl	(%esi), %eax
	movl	%eax, 4(%esp)
	movl	8(%ebp), %eax
	movl	%eax, (%esp)
	call	L_scanPtr$stub
	movl	8(%ebp), %edx
	movl	%eax, (%esi)
	movl	L_eRANGE$non_lazy_ptr-"L00000000005$pb"(%ebx), %esi
	movl	(%esi), %eax
	movl	%edx, (%esp)
	movl	%eax, 4(%esp)
	call	L_scanPtr$stub
	movl	%eax, (%esi)
	movl	L_eNOPERM$non_lazy_ptr-"L00000000005$pb"(%ebx), %esi
	movl	(%esi), %eax
	movl	%eax, 4(%esp)
	movl	8(%ebp), %eax
	movl	%eax, (%esp)
	call	L_scanPtr$stub
	movl	8(%ebp), %edx
	movl	%eax, (%esi)
	movl	L_eNOFILE$non_lazy_ptr-"L00000000005$pb"(%ebx), %esi
	movl	(%esi), %eax
	movl	%edx, (%esp)
	movl	%eax, 4(%esp)
	call	L_scanPtr$stub
	movl	%eax, (%esi)
	movl	L_eNOTDIR$non_lazy_ptr-"L00000000005$pb"(%ebx), %esi
	movl	(%esi), %eax
	movl	%eax, 4(%esp)
	movl	8(%ebp), %eax
	movl	%eax, (%esp)
	call	L_scanPtr$stub
	movl	8(%ebp), %edx
	movl	%eax, (%esi)
	movl	L_eCFGERR$non_lazy_ptr-"L00000000005$pb"(%ebx), %esi
	movl	(%esi), %eax
	movl	%edx, (%esp)
	movl	%eax, 4(%esp)
	call	L_scanPtr$stub
	movl	%eax, (%esi)
	movl	L_eEOF$non_lazy_ptr-"L00000000005$pb"(%ebx), %esi
	movl	(%esi), %eax
	movl	%eax, 4(%esp)
	movl	8(%ebp), %eax
	movl	%eax, (%esp)
	call	L_scanPtr$stub
	movl	8(%ebp), %edx
	movl	%eax, (%esi)
	movl	L_eIOERROR$non_lazy_ptr-"L00000000005$pb"(%ebx), %esi
	movl	(%esi), %eax
	movl	%edx, (%esp)
	movl	%eax, 4(%esp)
	call	L_scanPtr$stub
	movl	%eax, (%esi)
	movl	L_eABORT$non_lazy_ptr-"L00000000005$pb"(%ebx), %esi
	movl	(%esi), %eax
	movl	%eax, 4(%esp)
	movl	8(%ebp), %eax
	movl	%eax, (%esp)
	call	L_scanPtr$stub
	movl	8(%ebp), %edx
	movl	%eax, (%esi)
	movl	L_eNOTFND$non_lazy_ptr-"L00000000005$pb"(%ebx), %esi
	movl	(%esi), %eax
	movl	%edx, (%esp)
	movl	%eax, 4(%esp)
	call	L_scanPtr$stub
	movl	%eax, (%esi)
	movl	L_eCONNECT$non_lazy_ptr-"L00000000005$pb"(%ebx), %esi
	movl	(%esi), %eax
	movl	%eax, 4(%esp)
	movl	8(%ebp), %eax
	movl	%eax, (%esp)
	call	L_scanPtr$stub
	movl	8(%ebp), %edx
	movl	%eax, (%esi)
	movl	L_eFAIL$non_lazy_ptr-"L00000000005$pb"(%ebx), %esi
	movl	(%esi), %eax
	movl	%edx, (%esp)
	movl	%eax, 4(%esp)
	call	L_scanPtr$stub
	movl	%eax, (%esi)
	movl	L_eINVCODE$non_lazy_ptr-"L00000000005$pb"(%ebx), %esi
	movl	(%esi), %eax
	movl	%eax, 4(%esp)
	movl	8(%ebp), %eax
	movl	%eax, (%esp)
	call	L_scanPtr$stub
	movl	8(%ebp), %edx
	movl	%eax, (%esi)
	movl	L_eASSIGN$non_lazy_ptr-"L00000000005$pb"(%ebx), %esi
	movl	(%esi), %eax
	movl	%edx, (%esp)
	movl	%eax, 4(%esp)
	call	L_scanPtr$stub
	movl	%eax, (%esi)
	movl	L_eSYSTEM$non_lazy_ptr-"L00000000005$pb"(%ebx), %esi
	movl	(%esi), %eax
	movl	%eax, 4(%esp)
	movl	8(%ebp), %eax
	movl	%eax, (%esp)
	call	L_scanPtr$stub
	movl	8(%ebp), %edx
	movl	%eax, (%esi)
	movl	L_eDEAD$non_lazy_ptr-"L00000000005$pb"(%ebx), %esi
	movl	(%esi), %eax
	movl	%edx, (%esp)
	movl	%eax, 4(%esp)
	call	L_scanPtr$stub
	movl	%eax, (%esi)
	movl	L_eTIME$non_lazy_ptr-"L00000000005$pb"(%ebx), %esi
	movl	(%esi), %eax
	movl	%eax, 4(%esp)
	movl	8(%ebp), %eax
	movl	%eax, (%esp)
	call	L_scanPtr$stub
	movl	8(%ebp), %edx
	movl	%eax, (%esi)
	movl	L_eDUPLICATE$non_lazy_ptr-"L00000000005$pb"(%ebx), %esi
	movl	(%esi), %eax
	movl	%edx, (%esp)
	movl	%eax, 4(%esp)
	call	L_scanPtr$stub
	movl	%eax, (%esi)
	movl	L_eNOIMPL$non_lazy_ptr-"L00000000005$pb"(%ebx), %esi
	movl	(%esi), %eax
	movl	%eax, 4(%esp)
	movl	8(%ebp), %eax
	movl	%eax, (%esp)
	call	L_scanPtr$stub
	movl	8(%ebp), %edx
	movl	%eax, (%esi)
	movl	L_eNOTENUF$non_lazy_ptr-"L00000000005$pb"(%ebx), %esi
	movl	(%esi), %eax
	movl	%edx, (%esp)
	movl	%eax, 4(%esp)
	call	L_scanPtr$stub
	movl	%eax, (%esi)
	movl	L_eINTRUPT$non_lazy_ptr-"L00000000005$pb"(%ebx), %esi
	movl	(%esi), %eax
	movl	%eax, 4(%esp)
	movl	8(%ebp), %eax
	movl	%eax, (%esp)
	call	L_scanPtr$stub
	movl	%eax, (%esi)
	addl	$44, %esp
	popl	%ebx
	popl	%esi
	popl	%edi
	leave
	ret
	.align 4,0x90
_syCompFun:
	pushl	%ebp
	movl	%esp, %ebp
	subl	$24, %esp
	movl	%esi, -4(%ebp)
	movl	16(%ebp), %ecx
	movl	12(%ebp), %esi
	movl	%ebx, -8(%ebp)
	call	L41
"L00000000006$pb":
L41:
	popl	%ebx
	cmpl	%ecx, %esi
	je	L30
	movl	L_symbolClass$non_lazy_ptr-"L00000000006$pb"(%ebx), %eax
	movl	(%esi), %edx
	cmpl	(%eax), %edx
	jne	L32
	cmpl	(%ecx), %edx
	je	L40
L32:
	movl	$3, %eax
L38:
	movl	-8(%ebp), %ebx
	movl	-4(%ebp), %esi
	leave
	ret
	.align 4,0x90
L40:
	leal	4(%ecx), %eax
	movl	%eax, 4(%esp)
	leal	4(%esi), %eax
	movl	%eax, (%esp)
	call	L_uniCmp$stub
	cmpl	$0, %eax
	je	L30
	movl	-8(%ebp), %ebx
	sarl	$31, %eax
	movl	-4(%ebp), %esi
	notl	%eax
	leave
	andl	$2, %eax
	ret
L30:
	movl	$1, %eax
	jmp	L38
	.align 4,0x90
_syOutFun:
	pushl	%ebp
	movl	%esp, %ebp
	pushl	%edi
	pushl	%esi
	subl	$32, %esp
	movl	16(%ebp), %esi
	movl	12(%ebp), %edi
	leal	4(%esi), %eax
	movl	%eax, -12(%ebp)
	movl	$39, 4(%esp)
	movl	%edi, (%esp)
	call	L_outChar$stub
	testl	%eax, %eax
	jne	L43
	movzwl	4(%esi), %eax
	testw	%ax, %ax
	je	L48
	movl	-12(%ebp), %esi
	.align 4,0x90
L46:
	movzwl	%ax, %eax
	movl	%eax, 4(%esp)
	movl	%edi, (%esp)
	call	L_wStringChr$stub
	testl	%eax, %eax
	jne	L43
	movzwl	2(%esi), %eax
	addl	$2, %esi
	testw	%ax, %ax
	jne	L46
L48:
	movl	$39, 12(%ebp)
	movl	%edi, 8(%ebp)
	addl	$32, %esp
	popl	%esi
	popl	%edi
	leave
	jmp	L_outChar$stub
	.align 4,0x90
L43:
	addl	$32, %esp
	popl	%esi
	popl	%edi
	leave
	ret
	.align 4,0x90
_syHashFun:
	pushl	%ebp
	movl	%esp, %ebp
	movl	12(%ebp), %eax
	addl	$4, %eax
	movl	%eax, 8(%ebp)
	leave
	jmp	L_uniHash$stub
	.align 4,0x90
.globl _installSymbol
_installSymbol:
	pushl	%ebp
	movl	%esp, %ebp
	subl	$40, %esp
	movl	%ebx, -12(%ebp)
	call	L60
"L00000000007$pb":
L60:
	popl	%ebx
	movl	%esi, -8(%ebp)
	movl	8(%ebp), %esi
	movl	%edi, -4(%ebp)
	leal	4(%esi), %edi
	movl	_dictionary-"L00000000007$pb"(%ebx), %eax
	movl	%edi, (%esp)
	movl	%eax, 4(%esp)
	call	L_Search$stub
	andl	$-4, %eax
	jne	L59
	movl	_dictionary-"L00000000007$pb"(%ebx), %eax
	orl	$1, %esi
	movl	%esi, 4(%esp)
	movl	%edi, (%esp)
	movl	%eax, 8(%esp)
	call	L_Install$stub
L59:
	movl	-12(%ebp), %ebx
	movl	-8(%ebp), %esi
	movl	-4(%ebp), %edi
	leave
	ret
	.align 4,0x90
_syCopyFun:
	pushl	%ebp
	movl	%esp, %ebp
	subl	$24, %esp
	movl	%esi, -8(%ebp)
	movl	16(%ebp), %esi
	movl	%edi, -4(%ebp)
	leal	4(%esi), %eax
	movl	%eax, (%esp)
	call	L_uniStrLen$stub
	movl	%esi, 4(%esp)
	leal	9(%eax,%eax), %edi
	movl	12(%ebp), %eax
	andl	$-4, %edi
	movl	%edi, 8(%esp)
	movl	%eax, (%esp)
	call	L_memmove$stub
	movl	12(%ebp), %edx
	movl	-8(%ebp), %esi
	leal	(%edi,%edx), %eax
	movl	-4(%ebp), %edi
	leave
	ret
	.align 4,0x90
.globl _newSymbol
_newSymbol:
	pushl	%ebp
	movl	%esp, %ebp
	subl	$56, %esp
	movl	%ebx, -12(%ebp)
	call	L67
"L00000000008$pb":
L67:
	popl	%ebx
	movl	%esi, -8(%ebp)
	movl	8(%ebp), %esi
	movl	%edi, -4(%ebp)
	movl	%esp, -32(%ebp)
	movl	%esi, (%esp)
	call	L_strlen$stub
	incl	%eax
	leal	30(%eax,%eax), %edx
	andl	$-16, %edx
	subl	%edx, %esp
	leal	16(%esp), %edi
	movl	%esi, (%esp)
	movl	%eax, 8(%esp)
	movl	%edi, 4(%esp)
	call	L__uni$stub
	movl	_dictionary-"L00000000008$pb"(%ebx), %eax
	movl	%edi, (%esp)
	movl	%eax, 4(%esp)
	call	L_Search$stub
	testl	$-4, %eax
	movl	%eax, %esi
	jne	L64
	movl	%edi, (%esp)
	call	L_uniStrLen$stub
	addl	%eax, %eax
	movl	%eax, -28(%ebp)
	addl	$9, %eax
	shrl	$2, %eax
	movl	%eax, (%esp)
	call	L_permAllocate$stub
	movl	%eax, %esi
	movl	L_symbolClass$non_lazy_ptr-"L00000000008$pb"(%ebx), %eax
	movl	(%eax), %eax
	movl	%eax, (%esi)
	movl	-28(%ebp), %edx
	leal	4(%esi), %eax
	movl	%edi, 4(%esp)
	orl	$1, %esi
	movl	%eax, (%esp)
	addl	$2, %edx
	movl	%edx, 8(%esp)
	call	L_memcpy$stub
	movl	_dictionary-"L00000000008$pb"(%ebx), %eax
	movl	%esi, 4(%esp)
	movl	%eax, 8(%esp)
	movl	%esi, %eax
	andl	$-4, %eax
	addl	$4, %eax
	movl	%eax, (%esp)
	call	L_Install$stub
L64:
	movl	%esi, %eax
	movl	-32(%ebp), %esp
	movl	-12(%ebp), %ebx
	movl	-8(%ebp), %esi
	movl	-4(%ebp), %edi
	leave
	ret
	.cstring
LC1:
	.ascii "\0"
LC2:
	.ascii "main\0"
LC3:
	.ascii "go.stdlib#rootThread\0"
LC4:
	.ascii "go.boot\0"
LC5:
	.ascii "$process\0"
LC6:
	.ascii "go.io#fifoSpecial\0"
LC7:
	.ascii "go.io#directory\0"
LC8:
	.ascii "go.io#charSpecial\0"
LC9:
	.ascii "go.io#blockSpecial\0"
LC10:
	.ascii "go.io#plainFile\0"
LC11:
	.ascii "go.io#symlink\0"
LC12:
	.ascii "go.io#socket\0"
LC13:
	.ascii "go.io#unknownFileType\0"
LC14:
	.ascii "$loaded\0"
LC15:
	.ascii "$version\0"
LC16:
	.ascii "*\0"
LC17:
	.ascii "$defined\0"
LC18:
	.ascii "$label\0"
LC19:
	.ascii "start_thread%0\0"
LC20:
	.ascii "go.stdlib@delayHandler\0"
	.text
	.align 4,0x90
.globl _initDict
_initDict:
	pushl	%ebp
	movl	%esp, %ebp
	pushl	%ebx
	call	L70
"L00000000009$pb":
L70:
	popl	%ebx
	subl	$20, %esp
	movl	$0, 12(%esp)
	movl	$256, (%esp)
	movl	L_uniCmp$non_lazy_ptr-"L00000000009$pb"(%ebx), %eax
	movl	%eax, 8(%esp)
	movl	L_uniHash$non_lazy_ptr-"L00000000009$pb"(%ebx), %eax
	movl	%eax, 4(%esp)
	call	L_NewHash$stub
	movl	%eax, _dictionary-"L00000000009$pb"(%ebx)
	call	L_standardClasses$stub
	call	L_initErrorSymbols$stub
	leal	LC1-"L00000000009$pb"(%ebx), %eax
	movl	%eax, (%esp)
	call	_newSymbol
	movl	L_emptySymbol$non_lazy_ptr-"L00000000009$pb"(%ebx), %edx
	movl	%eax, (%edx)
	movl	$0, (%esp)
	movl	$0, 4(%esp)
	call	L_permInteger$stub
	movl	L_zero$non_lazy_ptr-"L00000000009$pb"(%ebx), %edx
	movl	%eax, (%edx)
	leal	LC2-"L00000000009$pb"(%ebx), %eax
	movl	$1, 4(%esp)
	movl	%eax, (%esp)
	call	L_newProgLbl$stub
	movl	L_kmain$non_lazy_ptr-"L00000000009$pb"(%ebx), %edx
	movl	%eax, (%edx)
	leal	LC3-"L00000000009$pb"(%ebx), %eax
	movl	%eax, (%esp)
	call	L_newEnumSym$stub
	movl	L_kmainThread$non_lazy_ptr-"L00000000009$pb"(%ebx), %edx
	movl	%eax, (%edx)
	leal	LC4-"L00000000009$pb"(%ebx), %eax
	movl	%eax, (%esp)
	call	_newSymbol
	movl	L_bootProg$non_lazy_ptr-"L00000000009$pb"(%ebx), %edx
	movl	%eax, (%edx)
	leal	LC5-"L00000000009$pb"(%ebx), %eax
	movl	%eax, (%esp)
	call	_newSymbol
	movl	L_kprocessFlag$non_lazy_ptr-"L00000000009$pb"(%ebx), %edx
	movl	%eax, (%edx)
	leal	LC6-"L00000000009$pb"(%ebx), %eax
	movl	%eax, (%esp)
	call	L_newEnumSym$stub
	movl	L_kfifo$non_lazy_ptr-"L00000000009$pb"(%ebx), %edx
	movl	%eax, (%edx)
	leal	LC7-"L00000000009$pb"(%ebx), %eax
	movl	%eax, (%esp)
	call	L_newEnumSym$stub
	movl	L_kdir$non_lazy_ptr-"L00000000009$pb"(%ebx), %edx
	movl	%eax, (%edx)
	leal	LC8-"L00000000009$pb"(%ebx), %eax
	movl	%eax, (%esp)
	call	L_newEnumSym$stub
	movl	L_kcharfile$non_lazy_ptr-"L00000000009$pb"(%ebx), %edx
	movl	%eax, (%edx)
	leal	LC9-"L00000000009$pb"(%ebx), %eax
	movl	%eax, (%esp)
	call	L_newEnumSym$stub
	movl	L_kblock$non_lazy_ptr-"L00000000009$pb"(%ebx), %edx
	movl	%eax, (%edx)
	leal	LC10-"L00000000009$pb"(%ebx), %eax
	movl	%eax, (%esp)
	call	L_newEnumSym$stub
	movl	L_kplain$non_lazy_ptr-"L00000000009$pb"(%ebx), %edx
	movl	%eax, (%edx)
	leal	LC11-"L00000000009$pb"(%ebx), %eax
	movl	%eax, (%esp)
	call	L_newEnumSym$stub
	movl	L_ksymlink$non_lazy_ptr-"L00000000009$pb"(%ebx), %edx
	movl	%eax, (%edx)
	leal	LC12-"L00000000009$pb"(%ebx), %eax
	movl	%eax, (%esp)
	call	L_newEnumSym$stub
	movl	L_ksock$non_lazy_ptr-"L00000000009$pb"(%ebx), %edx
	movl	%eax, (%edx)
	leal	LC13-"L00000000009$pb"(%ebx), %eax
	movl	%eax, (%esp)
	call	L_newEnumSym$stub
	movl	L_kunknown$non_lazy_ptr-"L00000000009$pb"(%ebx), %edx
	movl	%eax, (%edx)
	leal	LC14-"L00000000009$pb"(%ebx), %eax
	movl	%eax, (%esp)
	call	_newSymbol
	movl	L_kloadflag$non_lazy_ptr-"L00000000009$pb"(%ebx), %edx
	movl	%eax, (%edx)
	leal	LC15-"L00000000009$pb"(%ebx), %eax
	movl	%eax, (%esp)
	call	_newSymbol
	movl	L_kversion$non_lazy_ptr-"L00000000009$pb"(%ebx), %edx
	movl	%eax, (%edx)
	leal	LC16-"L00000000009$pb"(%ebx), %eax
	movl	%eax, (%esp)
	call	_newSymbol
	movl	L_universal$non_lazy_ptr-"L00000000009$pb"(%ebx), %edx
	movl	%eax, (%edx)
	leal	LC17-"L00000000009$pb"(%ebx), %eax
	movl	%eax, (%esp)
	call	_newSymbol
	movl	L_kdefined$non_lazy_ptr-"L00000000009$pb"(%ebx), %edx
	movl	%eax, (%edx)
	leal	LC18-"L00000000009$pb"(%ebx), %eax
	movl	%eax, (%esp)
	call	_newSymbol
	movl	L_klabel$non_lazy_ptr-"L00000000009$pb"(%ebx), %edx
	movl	%eax, (%edx)
	leal	LC19-"L00000000009$pb"(%ebx), %eax
	movl	%eax, (%esp)
	call	L_newEnumSym$stub
	movl	L_kstart$non_lazy_ptr-"L00000000009$pb"(%ebx), %edx
	movl	%eax, (%edx)
	leal	LC20-"L00000000009$pb"(%ebx), %eax
	movl	$1, 4(%esp)
	movl	%eax, (%esp)
	call	L_newProgLbl$stub
	movl	L_kdelay$non_lazy_ptr-"L00000000009$pb"(%ebx), %edx
	movl	%eax, (%edx)
	addl	$20, %esp
	popl	%ebx
	leave
	ret
	.align 4,0x90
.globl _newUniSymbol
_newUniSymbol:
	pushl	%ebp
	movl	%esp, %ebp
	subl	$40, %esp
	movl	%ebx, -12(%ebp)
	call	L75
"L00000000010$pb":
L75:
	popl	%ebx
	movl	%esi, -8(%ebp)
	movl	%edi, -4(%ebp)
	movl	_dictionary-"L00000000010$pb"(%ebx), %eax
	movl	%eax, 4(%esp)
	movl	8(%ebp), %eax
	movl	%eax, (%esp)
	call	L_Search$stub
	testl	$-4, %eax
	movl	%eax, %esi
	jne	L72
	movl	8(%ebp), %edx
	movl	%edx, (%esp)
	call	L_uniStrLen$stub
	leal	(%eax,%eax), %edi
	leal	9(%edi), %eax
	shrl	$2, %eax
	movl	%eax, (%esp)
	call	L_permAllocate$stub
	leal	2(%edi), %edx
	movl	%eax, %esi
	movl	L_symbolClass$non_lazy_ptr-"L00000000010$pb"(%ebx), %eax
	movl	(%eax), %eax
	movl	%eax, (%esi)
	leal	4(%esi), %eax
	orl	$1, %esi
	movl	%edx, 8(%esp)
	movl	8(%ebp), %edx
	movl	%eax, (%esp)
	movl	%edx, 4(%esp)
	call	L_memcpy$stub
	movl	_dictionary-"L00000000010$pb"(%ebx), %eax
	movl	%esi, 4(%esp)
	movl	%eax, 8(%esp)
	movl	%esi, %eax
	andl	$-4, %eax
	addl	$4, %eax
	movl	%eax, (%esp)
	call	L_Install$stub
L72:
	movl	%esi, %eax
	movl	-12(%ebp), %ebx
	movl	-8(%ebp), %esi
	movl	-4(%ebp), %edi
	leave
	ret
.lcomm _dictionary,4,2
.comm _varClass,4,2
.comm _suspClass,4,2
.comm _kvoid,4,2
.comm _emptySymbol,4,2
.comm _zero,4,2
.comm _bootProg,4,2
.comm _dieProg,4,2
.comm _exitProg,4,2
.comm _trapProg,4,2
.comm _kprocessFlag,4,2
.comm _kfifo,4,2
.comm _kdir,4,2
.comm _kcharfile,4,2
.comm _kblock,4,2
.comm _kplain,4,2
.comm _ksymlink,4,2
.comm _ksock,4,2
.comm _kunknown,4,2
.comm _kloadflag,4,2
.comm _kversion,4,2
.comm _universal,4,2
.comm _kdefined,4,2
.comm _klabel,4,2
.comm _kmain,4,2
.comm _kmainThread,4,2
.comm _kstart,4,2
.comm _kdelay,4,2
.comm _doResume,256,5
	.section __IMPORT,__pointers,non_lazy_symbol_pointers
L_eVARNEEDD$non_lazy_ptr:
	.indirect_symbol _eVARNEEDD
	.long	0
L_kmain$non_lazy_ptr:
	.indirect_symbol _kmain
	.long	0
L_eIOERROR$non_lazy_ptr:
	.indirect_symbol _eIOERROR
	.long	0
L_zero$non_lazy_ptr:
	.indirect_symbol _zero
	.long	0
	.section __IMPORT,__jump_table,symbol_stubs,self_modifying_code+pure_instructions,5
L_memmove$stub:
	.indirect_symbol _memmove
	hlt ; hlt ; hlt ; hlt ; hlt
	.section __IMPORT,__pointers,non_lazy_symbol_pointers
L_kvoid$non_lazy_ptr:
	.indirect_symbol _kvoid
	.long	0
L_klabel$non_lazy_ptr:
	.indirect_symbol _klabel
	.long	0
L_eABORT$non_lazy_ptr:
	.indirect_symbol _eABORT
	.long	0
L_eTPLNEEDD$non_lazy_ptr:
	.indirect_symbol _eTPLNEEDD
	.long	0
L_kdelay$non_lazy_ptr:
	.indirect_symbol _kdelay
	.long	0
L_ksymlink$non_lazy_ptr:
	.indirect_symbol _ksymlink
	.long	0
	.section __IMPORT,__jump_table,symbol_stubs,self_modifying_code+pure_instructions,5
L_permInteger$stub:
	.indirect_symbol _permInteger
	hlt ; hlt ; hlt ; hlt ; hlt
L_markStandardClasses$stub:
	.indirect_symbol _markStandardClasses
	hlt ; hlt ; hlt ; hlt ; hlt
	.section __IMPORT,__pointers,non_lazy_symbol_pointers
L_kversion$non_lazy_ptr:
	.indirect_symbol _kversion
	.long	0
L_kdefined$non_lazy_ptr:
	.indirect_symbol _kdefined
	.long	0
L_eFAIL$non_lazy_ptr:
	.indirect_symbol _eFAIL
	.long	0
	.section __IMPORT,__jump_table,symbol_stubs,self_modifying_code+pure_instructions,5
L_uniStrLen$stub:
	.indirect_symbol _uniStrLen
	hlt ; hlt ; hlt ; hlt ; hlt
	.section __IMPORT,__pointers,non_lazy_symbol_pointers
L_eNOTENUF$non_lazy_ptr:
	.indirect_symbol _eNOTENUF
	.long	0
L_eINSUFARG$non_lazy_ptr:
	.indirect_symbol _eINSUFARG
	.long	0
L_kmainThread$non_lazy_ptr:
	.indirect_symbol _kmainThread
	.long	0
L_eINVAL$non_lazy_ptr:
	.indirect_symbol _eINVAL
	.long	0
	.section __IMPORT,__jump_table,symbol_stubs,self_modifying_code+pure_instructions,5
L_newEnumSym$stub:
	.indirect_symbol _newEnumSym
	hlt ; hlt ; hlt ; hlt ; hlt
	.section __IMPORT,__pointers,non_lazy_symbol_pointers
L_globalSpace$non_lazy_ptr:
	.indirect_symbol _globalSpace
	.long	0
	.section __IMPORT,__jump_table,symbol_stubs,self_modifying_code+pure_instructions,5
L_NewHash$stub:
	.indirect_symbol _NewHash
	hlt ; hlt ; hlt ; hlt ; hlt
	.section __IMPORT,__pointers,non_lazy_symbol_pointers
L_dieProg$non_lazy_ptr:
	.indirect_symbol _dieProg
	.long	0
L_universal$non_lazy_ptr:
	.indirect_symbol _universal
	.long	0
L_eINTNEEDD$non_lazy_ptr:
	.indirect_symbol _eINTNEEDD
	.long	0
L_exitProg$non_lazy_ptr:
	.indirect_symbol _exitProg
	.long	0
L_eNOFILE$non_lazy_ptr:
	.indirect_symbol _eNOFILE
	.long	0
L_eSYSTEM$non_lazy_ptr:
	.indirect_symbol _eSYSTEM
	.long	0
	.section __IMPORT,__jump_table,symbol_stubs,self_modifying_code+pure_instructions,5
L_initErrorSymbols$stub:
	.indirect_symbol _initErrorSymbols
	hlt ; hlt ; hlt ; hlt ; hlt
L_wStringChr$stub:
	.indirect_symbol _wStringChr
	hlt ; hlt ; hlt ; hlt ; hlt
	.section __IMPORT,__pointers,non_lazy_symbol_pointers
L_eNOPERM$non_lazy_ptr:
	.indirect_symbol _eNOPERM
	.long	0
	.section __IMPORT,__jump_table,symbol_stubs,self_modifying_code+pure_instructions,5
L_DelHash$stub:
	.indirect_symbol _DelHash
	hlt ; hlt ; hlt ; hlt ; hlt
L_restartChars$stub:
	.indirect_symbol _restartChars
	hlt ; hlt ; hlt ; hlt ; hlt
	.section __IMPORT,__pointers,non_lazy_symbol_pointers
L_eNOTFND$non_lazy_ptr:
	.indirect_symbol _eNOTFND
	.long	0
L_eINTRUPT$non_lazy_ptr:
	.indirect_symbol _eINTRUPT
	.long	0
	.section __IMPORT,__jump_table,symbol_stubs,self_modifying_code+pure_instructions,5
L_scanPtr$stub:
	.indirect_symbol _scanPtr
	hlt ; hlt ; hlt ; hlt ; hlt
	.section __IMPORT,__pointers,non_lazy_symbol_pointers
L_trapProg$non_lazy_ptr:
	.indirect_symbol _trapProg
	.long	0
L_eCONNECT$non_lazy_ptr:
	.indirect_symbol _eCONNECT
	.long	0
	.section __IMPORT,__jump_table,symbol_stubs,self_modifying_code+pure_instructions,5
L_standardClasses$stub:
	.indirect_symbol _standardClasses
	hlt ; hlt ; hlt ; hlt ; hlt
	.section __IMPORT,__pointers,non_lazy_symbol_pointers
L_eNOTDIR$non_lazy_ptr:
	.indirect_symbol _eNOTDIR
	.long	0
L_eCFGERR$non_lazy_ptr:
	.indirect_symbol _eCFGERR
	.long	0
	.section __IMPORT,__jump_table,symbol_stubs,self_modifying_code+pure_instructions,5
L_Install$stub:
	.indirect_symbol _Install
	hlt ; hlt ; hlt ; hlt ; hlt
	.section __IMPORT,__pointers,non_lazy_symbol_pointers
L_kdir$non_lazy_ptr:
	.indirect_symbol _kdir
	.long	0
L_eUNIFY$non_lazy_ptr:
	.indirect_symbol _eUNIFY
	.long	0
L_eINVCODE$non_lazy_ptr:
	.indirect_symbol _eINVCODE
	.long	0
L_eSPACE$non_lazy_ptr:
	.indirect_symbol _eSPACE
	.long	0
	.section __IMPORT,__jump_table,symbol_stubs,self_modifying_code+pure_instructions,5
L_newSpecialClass$stub:
	.indirect_symbol _newSpecialClass
	hlt ; hlt ; hlt ; hlt ; hlt
	.section __IMPORT,__pointers,non_lazy_symbol_pointers
L_eNUMNEEDD$non_lazy_ptr:
	.indirect_symbol _eNUMNEEDD
	.long	0
	.section __IMPORT,__jump_table,symbol_stubs,self_modifying_code+pure_instructions,5
L_Uninstall$stub:
	.indirect_symbol _Uninstall
	hlt ; hlt ; hlt ; hlt ; hlt
	.section __IMPORT,__pointers,non_lazy_symbol_pointers
L_eEOF$non_lazy_ptr:
	.indirect_symbol _eEOF
	.long	0
	.section __IMPORT,__jump_table,symbol_stubs,self_modifying_code+pure_instructions,5
L_uniHash$stub:
	.indirect_symbol _uniHash
	hlt ; hlt ; hlt ; hlt ; hlt
	.section __IMPORT,__pointers,non_lazy_symbol_pointers
L_eSTRNEEDD$non_lazy_ptr:
	.indirect_symbol _eSTRNEEDD
	.long	0
L_kstart$non_lazy_ptr:
	.indirect_symbol _kstart
	.long	0
L_kunknown$non_lazy_ptr:
	.indirect_symbol _kunknown
	.long	0
	.section __IMPORT,__jump_table,symbol_stubs,self_modifying_code+pure_instructions,5
L_markPrograms$stub:
	.indirect_symbol _markPrograms
	hlt ; hlt ; hlt ; hlt ; hlt
	.section __IMPORT,__pointers,non_lazy_symbol_pointers
L_kplain$non_lazy_ptr:
	.indirect_symbol _kplain
	.long	0
L_eRANGE$non_lazy_ptr:
	.indirect_symbol _eRANGE
	.long	0
L_eDEAD$non_lazy_ptr:
	.indirect_symbol _eDEAD
	.long	0
	.section __IMPORT,__jump_table,symbol_stubs,self_modifying_code+pure_instructions,5
L_strlen$stub:
	.indirect_symbol _strlen
	hlt ; hlt ; hlt ; hlt ; hlt
	.section __IMPORT,__pointers,non_lazy_symbol_pointers
L_eASSIGN$non_lazy_ptr:
	.indirect_symbol _eASSIGN
	.long	0
L_kblock$non_lazy_ptr:
	.indirect_symbol _kblock
	.long	0
L_ksock$non_lazy_ptr:
	.indirect_symbol _ksock
	.long	0
	.section __IMPORT,__jump_table,symbol_stubs,self_modifying_code+pure_instructions,5
L__uni$stub:
	.indirect_symbol __uni
	hlt ; hlt ; hlt ; hlt ; hlt
	.section __IMPORT,__pointers,non_lazy_symbol_pointers
L_eDUPLICATE$non_lazy_ptr:
	.indirect_symbol _eDUPLICATE
	.long	0
L_eSYMNEEDD$non_lazy_ptr:
	.indirect_symbol _eSYMNEEDD
	.long	0
L_uniHash$non_lazy_ptr:
	.indirect_symbol _uniHash
	.long	0
L_eCODE$non_lazy_ptr:
	.indirect_symbol _eCODE
	.long	0
	.section __IMPORT,__jump_table,symbol_stubs,self_modifying_code+pure_instructions,5
L_outChar$stub:
	.indirect_symbol _outChar
	hlt ; hlt ; hlt ; hlt ; hlt
	.section __IMPORT,__pointers,non_lazy_symbol_pointers
L_kloadflag$non_lazy_ptr:
	.indirect_symbol _kloadflag
	.long	0
L_leftBase$non_lazy_ptr:
	.indirect_symbol _leftBase
	.long	0
L_emptySymbol$non_lazy_ptr:
	.indirect_symbol _emptySymbol
	.long	0
L_eLSTNEEDD$non_lazy_ptr:
	.indirect_symbol _eLSTNEEDD
	.long	0
L_kprocessFlag$non_lazy_ptr:
	.indirect_symbol _kprocessFlag
	.long	0
	.section __IMPORT,__jump_table,symbol_stubs,self_modifying_code+pure_instructions,5
L_Search$stub:
	.indirect_symbol _Search
	hlt ; hlt ; hlt ; hlt ; hlt
L_memcpy$stub:
	.indirect_symbol _memcpy
	hlt ; hlt ; hlt ; hlt ; hlt
	.section __IMPORT,__pointers,non_lazy_symbol_pointers
L_kfifo$non_lazy_ptr:
	.indirect_symbol _kfifo
	.long	0
L_eHANDLE$non_lazy_ptr:
	.indirect_symbol _eHANDLE
	.long	0
	.section __IMPORT,__jump_table,symbol_stubs,self_modifying_code+pure_instructions,5
L_newProgLbl$stub:
	.indirect_symbol _newProgLbl
	hlt ; hlt ; hlt ; hlt ; hlt
	.section __IMPORT,__pointers,non_lazy_symbol_pointers
L_uniCmp$non_lazy_ptr:
	.indirect_symbol _uniCmp
	.long	0
L_bootProg$non_lazy_ptr:
	.indirect_symbol _bootProg
	.long	0
	.section __IMPORT,__jump_table,symbol_stubs,self_modifying_code+pure_instructions,5
L_uniCmp$stub:
	.indirect_symbol _uniCmp
	hlt ; hlt ; hlt ; hlt ; hlt
	.section __IMPORT,__pointers,non_lazy_symbol_pointers
L_doResume$non_lazy_ptr:
	.indirect_symbol _doResume
	.long	0
L_eOCCUR$non_lazy_ptr:
	.indirect_symbol _eOCCUR
	.long	0
L_eTIME$non_lazy_ptr:
	.indirect_symbol _eTIME
	.long	0
	.section __IMPORT,__jump_table,symbol_stubs,self_modifying_code+pure_instructions,5
L_permAllocate$stub:
	.indirect_symbol _permAllocate
	hlt ; hlt ; hlt ; hlt ; hlt
	.section __IMPORT,__pointers,non_lazy_symbol_pointers
L_eCHRNEEDD$non_lazy_ptr:
	.indirect_symbol _eCHRNEEDD
	.long	0
L_symbolClass$non_lazy_ptr:
	.indirect_symbol _symbolClass
	.long	0
	.section __IMPORT,__jump_table,symbol_stubs,self_modifying_code+pure_instructions,5
L_ProcessTable$stub:
	.indirect_symbol _ProcessTable
	hlt ; hlt ; hlt ; hlt ; hlt
	.section __IMPORT,__pointers,non_lazy_symbol_pointers
L_kcharfile$non_lazy_ptr:
	.indirect_symbol _kcharfile
	.long	0
L_eNOIMPL$non_lazy_ptr:
	.indirect_symbol _eNOIMPL
	.long	0
L_eDIVZERO$non_lazy_ptr:
	.indirect_symbol _eDIVZERO
	.long	0
	.subsections_via_symbols
