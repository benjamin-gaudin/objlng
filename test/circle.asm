.text
	beqz $a0, init_end
	lw $a0, 0($a1)
	jal atoi
init_end:
	subi $sp, $sp, 4
	sw $v0, 0($sp)
	jal main
	li $v0, 10
	syscall
main:
	subi $sp, $sp, 4
	sw $fp, 0($sp)
	subi $sp, $sp, 4
	sw $ra, 0($sp)
	addi $fp, $sp, 4
	addi $sp, $sp, -4
	li $t2, 0
	sw $t2, -8($fp)
	li $t2, 10
	la $t1, retour
	sw $t2, 0($t1)
	li $t2, 32
	la $t1, espace
	sw $t2, 0($t1)
	b __main_0
__main_1:
	lw $t0, 4($fp)
	subi $sp, $sp, 4
	sw $t0, 0($sp)
	lw $t0, -8($fp)
	subi $sp, $sp, 4
	sw $t0, 0($sp)
	jal affiche_ligne
	addi $sp, $sp, 8
	la $t2, retour
	lw $t2, 0($t2)
	move $a0, $t2
	li $v0, 11
	syscall
	li $t2, 1
	lw $t3, -8($fp)
	add $t2, $t3, $t2
	sw $t2, -8($fp)
__main_0:
	li $t2, 1
	lw $t3, 4($fp)
	add $t2, $t3, $t2
	lw $t3, -8($fp)
	slt $t2, $t3, $t2
	bnez $t2, __main_1
	li $t0, 0
	addi $sp, $fp, -4
	lw $ra, 0($sp)
	addi $sp, $sp, 4
	lw $fp, 0($sp)
	addi $sp, $sp, 4
	jr $ra
affiche_ligne:
	subi $sp, $sp, 4
	sw $fp, 0($sp)
	subi $sp, $sp, 4
	sw $ra, 0($sp)
	addi $fp, $sp, 4
	addi $sp, $sp, -4
	li $t2, 0
	sw $t2, -8($fp)
	b __affiche_ligne_0
__affiche_ligne_1:
	lw $t2, 8($fp)
	lw $t3, 8($fp)
	mul $t2, $t3, $t2
	lw $t3, -8($fp)
	lw $t4, -8($fp)
	mul $t3, $t4, $t3
	lw $t4, 4($fp)
	lw $t5, 4($fp)
	mul $t4, $t5, $t4
	add $t3, $t4, $t3
	slt $t2, $t3, $t2
	bnez $t2, __affiche_ligne_2
	li $t2, 35
	move $a0, $t2
	li $v0, 11
	syscall
	b __affiche_ligne_3
__affiche_ligne_2:
	li $t2, 46
	move $a0, $t2
	li $v0, 11
	syscall
__affiche_ligne_3:
	la $t2, espace
	lw $t2, 0($t2)
	move $a0, $t2
	li $v0, 11
	syscall
	li $t2, 1
	lw $t3, -8($fp)
	add $t2, $t3, $t2
	sw $t2, -8($fp)
__affiche_ligne_0:
	li $t2, 1
	lw $t3, 8($fp)
	add $t2, $t3, $t2
	lw $t3, -8($fp)
	slt $t2, $t3, $t2
	bnez $t2, __affiche_ligne_1
	li $t0, 0
	addi $sp, $fp, -4
	lw $ra, 0($sp)
	addi $sp, $sp, 4
	lw $fp, 0($sp)
	addi $sp, $sp, 4
	jr $ra
#built-in atoi
atoi:
	li $v0, 0
atoi_loop:
	lbu $t0, 0($a0)
	beqz $t0, atoi_end
	addi $t0, $t0, -48
	bltz $t0, atoi_error
	bge $t0, 10, atoi_error
	mul $v0, $v0, 10
	add $v0, $v0, $t0
	addi $a0, $a0, 1
	b atoi_loop
atoi_error:
	li $v0, 10
	syscall
atoi_end:
	jr $ra
.data
retour:
	.word 0
espace:
	.word 0
