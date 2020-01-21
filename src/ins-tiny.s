
	INCLUDE "includes/PhotonsMiniWrapper1.04!.i"

********** Demo **********		;Demo-specific non-startup code below.

w	=320				;screen width, height, depth
h	=256
bpls	=1				;handy values:
bpl	=w/16*2				;byte-width of 1 bitplane line
bwid	=bpls*bpl			;byte-width of 1 pixel line (all bpls)

P61mode	=1
usecode	=-1
P61pl=usecode&$400000
split4	=0
splitchans=1
visuctrs=1
asmonereport	=0
p61system=0
p61exec	=0
p61fade	=0
channels=4
playflag=0
p61bigjtab=0
opt020	=0
p61jump	=0
C	=0
clraudxdat=0
optjmp	=1
oscillo	=0
quietstart=0
use1Fx=0

	ifeq P61mode-1
p61cia	=1
lev6	=1
noshorts=0
dupedec	=0
suppF01	=1
	endc

Demo:					;a4=VBR, a6=Custom Registers Base addr
	lea MulsTab(pc),a0
	move.w #256-1,d7
	moveq #0,d0
.init_mulstab:
	move.w d0,(a0)+
	add.w #bwid,d0
	dbf d7,.init_mulstab

    *--- init ---*
	move.l #VBint,$6c(a4)		;Set our new VERTB vector,
	move.w #$c020,$9a(a6)		;and enable the interrupt.
	move.w #$87c0,$96(a6)		;Enable Blitter, Bitplane, Copper DMA

	lea Module1,a0
	sub.l a1,a1
	sub.l a2,a2
	moveq #0,d0
	jsr P61_Init

	move.l DrawBuf(pc),a0			;pointer start addr
	moveq #bpl,d0			;pointer addr increment
	lea BplPtrs+2,a1		;start position to poke in copper
	moveq #bpls-1,d1		;pointer count-1
	bsr.w PokePtrs			;poke bitplane ptrs into copper list.

	lea.l textscr,a0			;pointer start addr
	moveq #1,d0			;pointer addr increment
	lea BplPtrs+2+8,a1		;start position to poke in copper
	moveq #1,d1		;pointer count-1
	bsr.w PokePtrs			;poke bitplane ptrs into copper list.

	move.l #Copper,$80(a6)		;Now set our initalized copper.

	move.w #0,textposx
	move.w #0,textposy
    *--- main loop ---*
MainLoop:
    bsr DoubleBuffer

	move.l DrawBuf,a1			;Then, the screen
	bsr.w ClearScreen		;clear, Yoda pls.
	jsr WaitBlitter		;Wait out blit: we plot to same area

	; move.w #$00f,$180(a6)		;(Optional: shows rastertime left)

	movem.w XAngle(pc),d0-d2
	bsr.w InitRotate

	lea Cube(pc),a0
	lea ProjectedCube(pc),a1
	lea Sintab(pc),a2

	move.w ZCounter(pc),d3
	and.w #$3fe,d3
	move.w (a2,d3),d3
	asr.w #7,d3
	add.w #100,d3
	move.w d3,ObjZ

	moveq #12-1,d7
.rotate_loop:
	movem.w (a0)+,d0-d2
	bsr.w RotatePoint_000

	add.w ObjZ(pc),d2
	bsr _ProjPoint
	movem.w d0-d1,(a1)
	add.l #4,a1
	dbf d7,.rotate_loop

	bsr InterpolateOuter
	bsr InterpolateInner

	lea.l introtext(pc),a0
	move.w charpos(pc),d3
	lea.l font,a1
	lea.l textscr,a2
.charloop:
	move.b (a0,d3.w),d0
	tst.b d0
	beq.s .quitprint
	cmp.b #' ',d0
	bne.s .test_eol
	move.b #1,endofword
	addq.w #1,d3
	add.w #1,textposx
	bra.s .quitprint
.test_eol:
	cmp.b #10,d0
	bne.s .normal_char
	move.w #0,textposx
	add.w #40*8,textposy
	addq.w #1,d3
	bra .charloop
.normal_char:
	cmp.b #1,endofword
	beq.s .quitprint
	addq.w #1,d3
	sub.b  #' ',d0
	ext.w d0
	lsl.w #3,d0
	move.w textposx(pc),d2
	add.w textposy(pc),d2
	moveq #8-1,d6
.print:
	move.b (a1,d0.w),(a2,d2)
	add.w #1,d0
	add.w #40,d2
	dbf d6,.print
	add.w #1,textposx
	dbf d7,.charloop
.quitprint:
	move.w d3,charpos

	; move.w #$f00,$180(a6)		;(Optional: shows rastertime left)
	move.w #$12c,d0			;No buffering, so wait until raster
	bsr.w WaitRaster		;is below the Display Window.

	btst #6,$bfe001			;Left mouse button not pressed?
	bne MainLoop			;then loop 
    
	*--- exit ---*
	jsr P61_End
	rts				;else exit demo

	cnop 0,2
charpos: dc.w 0
textposx: dc.w 0
textposy: dc.w 0
endofword: dc.b 0

	cnop 0,4
InterpolateOuter:
	lea ProjectedCube(pc),a0
	lea Lines(pc),a1
	moveq #0,d4

	moveq #3-1,d7
.face_loop:
	lea InterpolatedCoords(pc),a2
	add.l d4,a2
	
	moveq #2-1,d6
.outer_loop:
	moveq #0,d0
	moveq #0,d1
	moveq #0,d2
	moveq #0,d3

	movem.w (a1)+,d1/d3
	move.w (a0,d1),d0
	move.w 2(a0,d1),d1
	move.w (a0,d3),d2
	move.w 2(a0,d3),d3

	swap d0
	swap d2
	sub.l d0,d2
	asr.l #3,d2

	swap d1
	swap d3
	sub.l d1,d3
	asr.l #3,d3

	moveq #8-1,d5
.interpolate:
	movem.l d0-d1,-(sp)

	movem.l d0-d1,(a2)
	add.l #16,a2

	swap d0
	ext.l d0
	swap d1
	ext.l d1

	movem.l (sp)+,d0-d1
	add.l d2,d0
	add.l d3,d1
	dbf d5,.interpolate

	lea InterpolatedCoords+8(pc),a2
	add.l d4,a2
	dbf d6,.outer_loop

	add.l #8*4*2*2,d4
	dbf d7,.face_loop
	rts


InterpolateInner:
	lea InterpolatedCoords(pc),a0
	lea MulsTab(pc),a2
	moveq #24-1,d7
.outer_loop:
	movem.l (a0)+,d0-d3

	sub.l d0,d2
	asr.l #3,d2

	sub.l d1,d3
	asr.l #3,d3

	moveq #8-1,d6
.interpolate:
	movem.l d0-d3,-(sp)

	swap d0
	ext.l d0
	swap d1
	ext.l d1

	moveq #1,d2
	move.l DrawBuf(pc),a1
	bsr.w Plot

	movem.l (sp)+,d0-d3
	
	add.l d2,d0
	add.l d3,d1
	dbf d6,.interpolate
	dbf d7,.outer_loop
	rts

VBint:					;Blank template VERTB interrupt
	movem.l d0/a6,-(sp)		;Save used registers, could have been
	lea $dff000,a6			;modified, so set custom base again.
	btst #5,$1f(a6)			;My level 3 interrupt?
	beq.s .notvb			;if not, skip

	add.w #6,XAngle
	add.w #2,YAngle
	add.w #4,ZAngle

	add.w #8,ZCounter

	add.w #1,CharTimer
	move.w CharTimer(pc),d0
	and.w #$f,d0
	tst d0
	bne.s .skip

	cmp.b #1,endofword
	bne.s .skip
	move.b #0,endofword
.skip:
	moveq #$20,d0			;VB interrupt bit
	move.w d0,$9c(a6)		;clear, Yoda pls.
	move.w d0,$9c(a6)		;twice, Yoda pls. (A4000 fix)
.notvb:	movem.l (sp)+,d0/a6
	rte

	cnop 0,4
CharTimer: dc.w 0

DoubleBuffer:
	lea DrawBuf(pc),a0
	lea ViewBuf(pc),a1
	move.l (a0),d0
	move.l (a1),(a0)
	move.l d0,(a1)

	move.l ViewBuf(pc),a0			;pointer start addr
	moveq #bpl,d0			;pointer addr increment
	lea BplPtrs+2,a1		;start position to poke in copper
	moveq #bpls-1,d1		;pointer count-1
	bsr.s PokePtrs			;poke bitplane ptrs into copper list.
	rts

PokePtrs:				;Generic, poke ptrs into copper list
.bpll:	move.l a0,d2
	swap d2
	move.w d2,(a1)			;high word of address
	move.w a0,4(a1)			;low word of address
	addq.w #8,a1			;skip two copper instructions
	add.l d0,a0			;next ptr
	dbf d1,.bpll
	rts

ClearScreen:				;a1=screen destination address to clear
	bsr.w WaitBlitter
	clr.w $66(a6)			;destination modulo
	move.l #$01000000,$40(a6)	;set operation type in BLTCON0/1
	move.l a1,$54(a6)		;destination address
	move.w #h*bpls*64+bpl/2,$58(a6)	;blitter operation size: 768x20 words
	rts

PlotPoints:				;a0=points, a1=screen, d0=count
	subq.w #1,d0			;Subtract 1 for dbf as is the custom.
.l:	movem.w (a0)+,d1-d3		;Just load the values for each dot,
	bsr.s Plot			;and call the generic Plot routine.
	dbf d0,.l			;Until all points drawn.
	rts

Plot:					;d1=x, d2=y, d3=color, a1=screen
	add.w ObjX(PC),d0		;Add position of the amazing
	add.w ObjY(PC),d1		;secret dot object!

	add.w d1,d1
	move.w (a2,d1.w),d1

	move.w d0,d3			;left-to-right x position,
	not.w d3			;to bit 7-0 (other bits unused by bset)
	asr.w #3,d0			;Byte offset for x position
	add.w d0,d1

	bset d3,(a1,d1.w)		;then set bit.

	rts

InitRotate:
	Movem.l	d0-a6,-(sp)
	Lea	Sintab(Pc),a0
	Lea	Costab(Pc),a1
	Lea	Rotatematrix(Pc),a2
	And.w	#$3fe,d0
	Add.w	d0,d0
	And.w	#$3fe,d1
	Add.w	d1,d1
	And.w	#$3fe,d2
	Add.w	d2,d2
	Move.w	(a0,d0.w),RotateSinx
	Move.w	(a1,d0.w),RotateCosx
	Move.w	(a0,d1.w),RotateSiny
	Move.w	(a1,d1.w),RotateCosy
	Move.w	(a0,d2.w),RotateSinz
	Move.w	(a1,d2.w),RotateCosz

	Move.w	RotateSinx(Pc),d2
	Muls.w	RotateSinz(Pc),d2
	Add.l	d2,d2
	Swap	d2

	Move.w	RotateSinx(Pc),d3
	Muls.w	RotateCosz(Pc),d3
	Add.l	d3,d3
	Swap	d3

	Move.w	RotateCosx(Pc),d4
	Muls.w	RotateSinz(Pc),d4
	Add.l	d4,d4
	Swap	d4

	Move.w	RotateCosx(Pc),d5
	Muls.w	RotateCosz(Pc),d5
	Add.l	d5,d5
	Swap	d5

	Move.w	RotateCosy(Pc),d0
	Muls.w	RotateCosz(Pc),d0
	Add.l	d0,d0
	Swap	d0
	Move.w	d0,(a2)

	Move.w	d4,d0
	Move.w	d3,d1
	Muls.w	RotateSiny(Pc),d1
	Add.l	d1,d1
	Swap	d1
	Sub.w	d1,d0
	Move.w	d0,2(a2)

	Move.w	d2,d0
	Move.w	d5,d1
	Muls.w	RotateSiny(Pc),d1
	Add.l	d1,d1
	Swap	d1
	Add.w	d1,d0
	Move.w	d0,4(a2)

	Move.w	RotateCosy(Pc),d0
	Muls.w	RotateSinz(Pc),d0
	Add.l	d0,d0
	Swap	d0
	Neg.w	d0
	Move.w	d0,6(a2)

	Move.w	d5,d0				
	Move.w	d2,d1				
	Muls.w	RotateSiny(Pc),d1
	Add.l	d1,d1
	Swap	d1
	Add.w	d1,d0
	Move.w	d0,8(a2)

	Move.w	d3,d0				
	Move.w	d4,d1				
	Muls.w	RotateSiny(Pc),d1
	Add.l	d1,d1
	Swap	d1
	Sub.w	d1,d0
	Move.w	d0,10(a2)

	Move.w	RotateSiny(Pc),d0
	Neg.w	d0
	Move.w	d0,12(a2)

	Move.w	RotateSinx(Pc),d0
	Muls.w	RotateCosy(Pc),d0
	Add.l	d0,d0
	Swap	d0
	Neg.w	d0
	Move.w	d0,14(a2)

	Move.w	RotateCosx(Pc),d0
	Muls.w	RotateCosy(Pc),d0
	Add.l	d0,d0
	Swap	d0
	Move.w	d0,16(a2)

	Movem.l	(sp)+,d0-a6
	Rts

TmpMat:	Ds.w	3
NewMat:	Ds.w	3

RotatePoint_000:
	Movem.l	a0/a1/a2,-(sp)	;/d3/d4/d5/d7,-(sp)
	Lea.l	Rotatematrix(Pc),a0
	Lea.l	TmpMat(Pc),a1
	Movem.w	d0/d1/d2,(a1)
	Lea.l	NewMat(Pc),a2
	REPT	3
	Move.w	0(a0),d0
	Muls	0(a1),d0		; a*x
	swap	d0

	Move.w	2(a0),d1
	Muls	2(a1),d1		; b*y
	swap	d1
	Add.w	d1,d0

	Move.w	4(a0),d1
	Muls	4(a1),d1		; c*z
	swap	d1
	Add.w	d1,d0			; d0 = X,Y,Z
	Move.w	d0,(a2)+

	Addq.l	#6,a0			; Next row in matrix.
	ENDR
	Lea.l	NewMat(Pc),a2
	Movem.w	(a2)+,d0/d1/d2
	Movem.l	(sp)+,a0/a1/a2	;/d3/d4/d5/d7
	Rts

_ProjPoint:
	Ext.l	d0
	Lsl.l	#8,d0
	Divs.w	d2,d0

	Ext.l	d1
	Lsl.l	#8,d1
	Divs.w	d2,d1
	Rts

introtext: 
   ;dc.b '1234567890123456789012345678901234567890',0
    dc.b '----------------------------------------',10
	dc.b ' ',10
	dc.b '                INSANE',10
	dc.b ' ',10
	dc.b '          PRESENTS -- TINY --',10
	dc.b ' ',10
	dc.b '                  AT',10
	dc.b ' ',10
	dc.b '             COMPUSPHERE',10
	dc.b ' ',10
	dc.b '                 2018',10
	dc.b ' ',10
	dc.b ' ',10
	dc.b ' THIS IS A PARTY CODED COMPOFILLER DONE ',10
	dc.b '  IN ABOUT 7 HOURS FROM SCRATCH AFTER ',10
	dc.b '  A LOT OF YEARS WITHOUT DEMO CODING!',10
	dc.b ' ',10
	dc.b '  DONT TAKE IT TOO SERIOUSLY AND STAY',10
	dc.b '             ... INSANE ...',10
	dc.b ' ',10
	dc.b '       PROSPECT - MYGG - VEDDER',10
	dc.b '     AND MINOR DEBUGGING BY ORIGO',10
	dc.b ' ',10
	dc.b ' ',10
	dc.b ' GREETINGS TO EVERYONE AT THE PARTY AND ',10
	dc.b '       OFCOURSE EVERYONE NOT HERE ',10
	dc.b '       FOR KEEPING THE SCENE ALIVE',10
	dc.b ' ',10
	dc.b '      LOTS OF INSANE LOVE /PROSPECT ',10
	dc.b ' ',10
    dc.b '----------------------------------------',0

	cnop 0,4
RotateSinx:	Ds.w	1
RotateCosx:	Ds.w	1
RotateSiny:	Ds.w	1
RotateCosy:	Ds.w	1
RotateSinz:	Ds.w	1
RotateCosz:	Ds.w	1

Rotatematrix:	Ds.w	9

********** Fastmem Data **********

DrawBuf:      dc.l Screen
ViewBuf:      dc.l Screen2

ObjX:	dc.w 160
ObjY:	dc.w 128			
ObjZ:	dc.w 0

XAngle:		dc.w	0
YAngle:		dc.w	0
ZAngle:		dc.w	0

ZCounter:	dc.w	0

Cube:
	dc.w -50, -50, -50
	dc.w  50, -50, -50
	dc.w  50,  50, -50
	dc.w -50,  50, -50

	dc.w -50, -50, 0
	dc.w  50, -50, 0
	dc.w  50,  50, 0
	dc.w -50,  50, 0

	dc.w -50, -50,  50
	dc.w  50, -50,  50
	dc.w  50,  50,  50
	dc.w -50,  50,  50

Lines:
	dc.w 0*4, 1*4, 3*4, 2*4
	dc.w 4*4, 5*4, 7*4, 6*4
	dc.w 8*4, 9*4, 11*4, 10*4

RotatedCube:			ds.w 3*12
ProjectedCube:			ds.w 2*12
InterpolatedCoords:		ds.l 8*2*2*3

MulsTab:				ds.w 256

Sintab:	DC.L	$000000C9,$0192025B,$032403ED,$04B6057F,$06480711,$07D908A2,$096B0A33,$0AFB0BC4
	DC.L	$0C8C0D54,$0E1C0EE3,$0FAB1073,$113A1201,$12C8138F,$1455151C,$15E216A8,$176E1833
	DC.L	$18F919BE,$1A821B47,$1C0B1CCF,$1D931E57,$1F1A1FDD,$209F2161,$222322E5,$23A62467
	DC.L	$252825E8,$26A82767,$282628E5,$29A32A61,$2B1F2BDC,$2C992D55,$2E112ECC,$2F873041
	DC.L	$30FB31B5,$326E3327,$33DF3496,$354D3604,$36BA376F,$382438D9,$398C3A40,$3AF23BA5
	DC.L	$3C563D07,$3DB83E68,$3F173FC5,$40734121,$41CE427A,$432543D0,$447A4524,$45CD4675
	DC.L	$471C47C3,$4869490F,$49B34A58,$4AFB4B9D,$4C3F4CE0,$4D814E20,$4EBF4F5D,$4FFB5097
	DC.L	$513351CE,$52685302,$539A5432,$54C95560,$55F55689,$571D57B0,$584258D3,$596459F3
	DC.L	$5A825B0F,$5B9C5C28,$5CB35D3E,$5DC75E4F,$5ED75F5D,$5FE36068,$60EB616E,$61F06271
	DC.L	$62F16370,$63EE646B,$64E86563,$65DD6656,$66CF6746,$67BC6831,$68A66919,$698B69FC
	DC.L	$6A6D6ADC,$6B4A6BB7,$6C236C8E,$6CF86D61,$6DC96E30,$6E966EFA,$6F5E6FC1,$70227082
	DC.L	$70E27140,$719D71F9,$725472AE,$7307735E,$73B5740A,$745E74B2,$75047555,$75A475F3
	DC.L	$7641768D,$76D87722,$776B77B3,$77FA783F,$788378C6,$79087949,$798979C7,$7A057A41
	DC.L	$7A7C7AB6,$7AEE7B26,$7B5C7B91,$7BC57BF7,$7C297C59,$7C887CB6,$7CE27D0E,$7D387D61
	DC.L	$7D897DB0,$7DD57DF9,$7E1C7E3E,$7E5F7E7E,$7E9C7EB9,$7ED47EEF,$7F087F20,$7F377F4C
	DC.L	$7F617F74,$7F867F96,$7FA67FB4,$7FC17FCD,$7FD77FE0,$7FE87FEF,$7FF57FF9,$7FFC7FFE
	DC.L	$7FFF7FFE,$7FFC7FF9,$7FF57FEF,$7FE87FE0,$7FD77FCD,$7FC17FB4,$7FA67F96,$7F867F74
	DC.L	$7F617F4C,$7F377F20,$7F087EEF,$7ED47EB9,$7E9C7E7E,$7E5E7E3E,$7E1C7DF9,$7DD57DAF
	DC.L	$7D897D61,$7D387D0E,$7CE27CB6,$7C887C59,$7C287BF7,$7BC47B90,$7B5B7B25,$7AEE7AB5
	DC.L	$7A7B7A40,$7A0479C7,$79887949,$790878C6,$7883783F,$77F977B2,$776B7722,$76D8768C
	DC.L	$764075F2,$75A47554,$750374B1,$745E740A,$73B4735E,$730672AD,$725371F9,$719C713F
	DC.L	$70E17082,$70216FC0,$6F5D6EFA,$6E956E2F,$6DC86D60,$6CF76C8D,$6C226BB6,$6B496ADB
	DC.L	$6A6C69FC,$698A6918,$68A56831,$67BB6745,$66CE6656,$65DC6562,$64E7646B,$63ED636F
	DC.L	$62F06270,$61EF616D,$60EA6067,$5FE25F5C,$5ED65E4E,$5DC65D3D,$5CB25C27,$5B9B5B0E
	DC.L	$5A8159F2,$596358D2,$584157AF,$571C5688,$55F4555F,$54C85431,$53995301,$526751CD
	DC.L	$51325096,$4FFA4F5C,$4EBE4E1F,$4D804CDF,$4C3E4B9C,$4AFA4A56,$49B2490E,$486847C2
	DC.L	$471B4674,$45CC4523,$447943CF,$43244279,$41CC4120,$40723FC4,$3F163E66,$3DB73D06
	DC.L	$3C553BA3,$3AF13A3F,$398B38D7,$3823376E,$36B93602,$354C3495,$33DD3325,$326D31B4
	DC.L	$30FA3040,$2F862ECB,$2E0F2D54,$2C972BDB,$2B1E2A60,$29A228E4,$28252766,$26A725E7
	DC.L	$25272466,$23A522E4,$22222160,$209E1FDB,$1F181E55,$1D921CCE,$1C0A1B46,$1A8119BC
	DC.L	$18F71832,$176C16A7,$15E1151A,$1454138D,$12C71200,$11391071,$0FAA0EE2,$0E1A0D52
	DC.L	$0C8A0BC2,$0AFA0A32,$096908A1,$07D8070F,$0646057E,$04B503EC,$0323025A,$019100C8
	DC.L	$0000FF37,$FE6EFDA5,$FCDCFC13,$FB4AFA81,$F9B8F8EF,$F826F75E,$F695F5CD,$F504F43C
	DC.L	$F374F2AC,$F1E4F11C,$F055EF8D,$EEC6EDFF,$ED38EC71,$EBAAEAE4,$EA1EE958,$E892E7CC
	DC.L	$E707E642,$E57DE4B9,$E3F4E330,$E26DE1A9,$E0E6E023,$DF61DE9E,$DDDCDD1B,$DC59DB99
	DC.L	$DAD8DA18,$D958D898,$D7D9D71B,$D65CD59E,$D4E1D424,$D367D2AB,$D1EFD134,$D079CFBE
	DC.L	$CF04CE4B,$CD92CCD9,$CC21CB6A,$CAB3C9FC,$C946C891,$C7DCC727,$C673C5C0,$C50DC45B
	DC.L	$C3A9C2F8,$C248C198,$C0E9C03A,$BF8CBEDF,$BE32BD86,$BCDBBC30,$BB85BADC,$BA33B98B
	DC.L	$B8E3B83D,$B796B6F1,$B64CB5A8,$B505B462,$B3C1B31F,$B27FB1DF,$B141B0A2,$B005AF68
	DC.L	$AECDAE32,$AD97ACFE,$AC65ABCD,$AB36AAA0,$AA0BA976,$A8E3A850,$A7BEA72C,$A69CA60D
	DC.L	$A57EA4F0,$A464A3D8,$A34CA2C2,$A239A1B1,$A129A0A3,$A01D9F98,$9F149E92,$9E109D8F
	DC.L	$9D0F9C90,$9C129B94,$9B189A9D,$9A2399AA,$993198BA,$984497CE,$975A96E7,$96759603
	DC.L	$95939524,$94B69449,$93DD9372,$9308929F,$923791D0,$916A9106,$90A2903F,$8FDE8F7D
	DC.L	$8F1E8EC0,$8E638E07,$8DAC8D52,$8CF98CA2,$8C4B8BF6,$8BA18B4E,$8AFC8AAB,$8A5C8A0D
	DC.L	$89BF8973,$892888DE,$8895884D,$880687C1,$877D8739,$86F786B7,$86778639,$85FB85BF
	DC.L	$8584854A,$851284DA,$84A4846F,$843B8409,$83D783A7,$8378834A,$831D82F2,$82C8829F
	DC.L	$82778250,$822B8207,$81E481C2,$81A18182,$81648147,$812B8111,$80F880E0,$80C980B4
	DC.L	$809F808C,$807A806A,$805A804C,$803F8033,$80298020,$80188011,$800B8007,$80048002
	DC.L	$80018002,$80048007,$800B8011,$80188020,$80298034,$803F804C,$805A806A,$807A808C
	DC.L	$809F80B4,$80C980E0,$80F88111,$812C8147,$81648182,$81A281C2,$81E48207,$822B8251
	DC.L	$8277829F,$82C882F2,$831E834B,$837883A7,$83D88409,$843C8470,$84A584DB,$8512854B
	DC.L	$858585C0,$85FC8639,$867886B7,$86F8873A,$877D87C2,$8807884E,$889588DE,$89298974
	DC.L	$89C08A0E,$8A5C8AAC,$8AFD8B4F,$8BA28BF6,$8C4C8CA2,$8CFA8D53,$8DAD8E08,$8E648EC1
	DC.L	$8F1F8F7E,$8FDF9040,$90A39106,$916B91D1,$923892A0,$93099373,$93DE944A,$94B79525
	DC.L	$95949605,$967696E8,$975B97D0,$984598BB,$993299AB,$9A249A9E,$9B199B96,$9C139C91
	DC.L	$9D109D90,$9E119E93,$9F169F9A,$A01EA0A4,$A12AA1B2,$A23AA2C4,$A34EA3D9,$A465A4F2
	DC.L	$A57FA60E,$A69EA72E,$A7BFA851,$A8E4A978,$AA0CAAA2,$AB38ABCF,$AC67ACFF,$AD99AE33
	DC.L	$AECEAF6A,$B007B0A4,$B142B1E1,$B281B321,$B3C2B464,$B507B5AA,$B64EB6F3,$B798B83E
	DC.L	$B8E5B98D,$BA35BADE,$BB87BC31,$BCDCBD88,$BE34BEE1,$BF8EC03C,$C0EBC19A,$C24AC2FA
	DC.L	$C3ABC45D,$C50FC5C2,$C675C729,$C7DDC892,$C948C9FE,$CAB4CB6B,$CC23CCDB,$CD94CE4D
	DC.L	$CF06CFC0,$D07BD135,$D1F1D2AD,$D369D426,$D4E3D5A0,$D65ED71C,$D7DBD89A,$D95ADA1A
	DC.L	$DADADB9A,$DC5BDD1D,$DDDEDEA0,$DF62E025,$E0E8E1AB,$E26EE332,$E3F6E4BB,$E57FE644
	DC.L	$E709E7CE,$E894E95A,$EA20EAE6,$EBACEC73,$ED3AEE01,$EEC8EF8F,$F057F11E,$F1E6F2AE
	DC.L	$F376F43E,$F506F5CF,$F697F760,$F828F8F1,$F9BAFA83,$FB4CFC14,$FCDDFDA6,$FE70FF39
	DC.L	$000100CA,$0193025C,$032503EE,$04B70580,$06480711,$07DA08A3,$096B0A34,$0AFC0BC4
	DC.L	$0C8C0D54,$0E1C0EE4,$0FAC1073,$113A1202,$12C9138F,$1456151C,$15E316A9,$176E1834
	DC.L	$18F919BE,$1A831B48,$1C0C1CD0,$1D941E57,$1F1A1FDD,$20A02162,$222422E6,$23A72468
	DC.L	$252825E9,$26A82768,$282728E6,$29A42A62,$2B1F2BDD,$2C992D55,$2E112ECD,$2F883042
	DC.L	$30FC31B5,$326E3327,$33DF3497,$354E3604,$36BA3770,$382538D9,$398D3A40,$3AF33BA5
	DC.L	$3C573D08,$3DB83E68,$3F173FC6,$40744121,$41CE427A,$432643D1,$447B4524,$45CD4675
	DC.L	$471D47C4,$486A490F,$49B44A58,$4AFB4B9E,$4C404CE1,$4D814E21,$4EC04F5E,$4FFB5098
	DC.L	$513451CF,$52695302,$539B5433,$54CA5560,$55F5568A,$571E57B0,$584358D4,$596459F4
	DC.L	$5A825B10,$5B9D5C29,$5CB45D3E,$5DC75E50,$5ED75F5E,$5FE36068,$60EC616F,$61F16271
	DC.L	$62F16371,$63EF646C,$64E86563,$65DD6657,$66CF6746,$67BC6832,$68A66919,$698B69FD
	DC.L	$6A6D6ADC,$6B4A6BB7,$6C236C8E,$6CF86D61,$6DC96E30,$6E966EFB,$6F5E6FC1,$70227083
	DC.L	$70E27140,$719D71F9,$725472AE,$7307735F,$73B5740A,$745F74B2,$75047555,$75A575F3
	DC.L	$7641768D,$76D87722,$776B77B3,$77FA783F,$788378C7,$79097949,$798979C8,$7A057A41
	DC.L	$7A7C7AB6,$7AEE7B26,$7B5C7B91,$7BC57BF7,$7C297C59,$7C887CB6,$7CE37D0E,$7D387D61
	DC.L	$7D897DB0,$7DD57DF9,$7E1C7E3E,$7E5F7E7E,$7E9C7EB9,$7ED57EEF,$7F087F20,$7F377F4D
	DC.L	$7F617F74,$7F867F96,$7FA67FB4,$7FC17FCD,$7FD77FE0,$7FE87FEF,$7FF57FF9,$7FFC7FFE
SinTab2:
Costab	=	Sintab+512

	include "includes/P6112-Play.i"

*******************************************************************************
	SECTION ChipData,DATA_C		;declared data that must be in chipmem
*******************************************************************************

Copper:
	dc.w $1fc,0			;Slow fetch mode, remove if AGA demo.
	dc.w $8e,$2c81			;Standard display window top, left
	dc.w $90,$2cc1			;and bottom, right.
	dc.w $92,$38			;Standard bitplane dma fetch start
	dc.w $94,$d0			;and stop for standard screen.

	dc.w $108,0; bwid-bpl		;modulos
	dc.w $10a,0;bwid-bpl

	dc.w $102,0			;Scroll register (and playfield pri)

Palette:				;Palette from COLOR00 to COLOR07
	dc.w $180,$046			;black (red,green,blue=0,0,0)
	dc.w $182,$aaa			;blue
	dc.w $184,$eee			;green
	dc.w $186,$eee			;cyan (=green+blue)
	dc.w $188,$f00			;red
	dc.w $18a,$f0f			;magenta (=red+blue)
	dc.w $18c,$ff0			;yellow (=red+green)
	dc.w $18e,$fff			;white (red,green,blue=15,15,15)

BplPtrs:
	dc.w $e0,0			;set high
	dc.w $e2,0			;and low word of bitplane ptr 1
	dc.w $e4,0			;same for bitplane 2
	dc.w $e6,0
	dc.w $e8,0			;and bitplane 3
	dc.w $ea,0
	dc.w $100,2*$1000+$200	;enable bitplanes

	dc.w $ffdf,$fffe		;allow VPOS>$ff
	dc.w $ffff,$fffe		;magic value, signals end of copperlist


font:		incbin "gfx/vedderfont_8x528.raw"
Module1:	incbin "music/P61.prospect is king"	;CIA, usecode $1006bf5f

*******************************************************************************
	SECTION ChipBuffers,BSS_C	;BSS doesn't count toward exe size
*******************************************************************************

Screen:		ds.b h*bwid			;Define storage to reserve for screen
Screen2:	ds.b h*bwid			;Define storage to reserve for screen

textscr:	ds.b h*bwid
