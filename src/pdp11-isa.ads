package Pdp11.ISA is

   type Register_Index is range 0 .. 7;

   type FP_Register_Index is range 0 .. 5;

   type V_Register_Index is range 0 .. 3;

   type Mode_Type is (Register_Mode,
                      Autoincrement_Mode,
                      Autodecrement_Mode,
                      Index_Mode);

   type Operand_Type is
      record
         Mode     : Mode_Type := Register_Mode;
         Deferred : Boolean   := False;
         Register : Register_Index := 0;
      end record;

   function Is_Register_Operand
     (Operand : Operand_Type)
      return Boolean
   is (Operand.Mode = Register_Mode and then not Operand.Deferred);

   function Encode
     (Operand : Operand_Type;
      Offset  : Natural)
      return Word_16;

   type Instruction_Type is
     (I_MOV, I_CMP, I_BIT, I_BIC, I_BIS, I_ADD, I_SUB,
      I_MUL, I_DIV, I_ASH, I_ASHC, I_XOR, I_SOB,
      I_CLR, I_COM, I_INC, I_DEC, I_NEG, I_ADC, I_SBC, I_TST,
      I_ROR, I_ROL, I_ASR, I_ASL,
      I_MARK, I_MFPI, I_MTPI, I_SXT,
      I_MTPS, I_MFPD, I_MTPD, I_MFPS,
      I_BR, I_BEQ, I_BLT, I_BLE,
      I_BMI, I_BLOS, I_BVS, I_BCS,
      I_JMP, I_JSR, I_RTS,
      I_CCC, I_SCC,
      I_MULF, I_MODF, I_ADDF, I_LDF, I_SUBF, I_CMPF, I_STF, I_DIVF,
      I_CLRF, I_TSTF, I_ABSF, I_NEGF,
      I_LDV, I_STV,
      I_ADDV, I_SUBV, I_ABSV, I_MULFV,
      I_CLRV, I_NEGV, I_NORMV);

   subtype Double_Operand_Instruction is
     Instruction_Type range I_MOV .. I_SUB;

   subtype Register_Double_Operand_Instruction is
     Instruction_Type range I_MUL .. I_XOR;

   subtype Single_Operand_Instruction is
     Instruction_Type range I_CLR .. I_MFPS;

   subtype Sized_Single_Operand_Instruction is
     Instruction_Type range I_CLR .. I_ASL;

   subtype Unsized_Single_Operand_Instruction is
     Instruction_Type range I_MARK .. I_MFPS;

   subtype Operand_Instruction is
     Instruction_Type range I_MOV .. I_MFPS;

   subtype Branch_Instruction is
     Instruction_Type range I_BR .. I_BCS;

   subtype Floating_Point_Instruction is
     Instruction_Type range I_MULF .. I_NEGF;

   subtype Floating_Point_F1 is
     Floating_Point_Instruction range I_MULF .. I_DIVF;

   subtype Floating_Point_F2 is
     Floating_Point_Instruction range I_CLRF .. I_NEGF;

   subtype Vector_Instruction is
     Instruction_Type range I_LDV .. I_NORMV;

   subtype Vector_F1 is Vector_Instruction range I_LDV .. I_STV;
   subtype Vector_F2 is Vector_Instruction range I_ADDV .. I_MULFV;
   subtype Vector_F3 is Vector_Instruction range I_CLRV .. I_NORMV;

   function Has_Source_Operand
     (Instruction : Instruction_Type)
      return Boolean
   is (Instruction in Double_Operand_Instruction);

   function Has_Source_Register
     (Instruction : Instruction_Type)
      return Boolean
   is (Instruction in Register_Double_Operand_Instruction
       or else Instruction = I_JSR
       or else Instruction = I_RTS);

   function Has_Destination_Operand
     (Instruction : Instruction_Type)
      return Boolean
   is (Instruction in Double_Operand_Instruction
         | Sized_Single_Operand_Instruction
         | Unsized_Single_Operand_Instruction
         | I_JSR | I_JMP);

   function Has_Branch_Operand
     (Instruction : Instruction_Type)
      return Boolean
   is (Instruction in Branch_Instruction);

   type Microsecond_Duration is delta 0.001 range
     0.0 .. (2 ** 31 - 1) * 0.001;
   for Microsecond_Duration'Small use 0.001;

   function Basic_Timing
     (Instruction : Instruction_Type)
      return Microsecond_Duration;

   function Src_Operand_Timing
     (Operand : Operand_Type)
      return Microsecond_Duration;

   function Dst_Operand_Timing
     (Operand : Operand_Type)
      return Microsecond_Duration;

   type Instruction_Record is
      record
         Instruction  : Instruction_Type  := I_CCC;
         Word         : Boolean           := True;
         Undefined    : Boolean           := False;
         N, Z, V, C   : Boolean           := False;
         Negate       : Boolean           := False;
         Src, Dst     : Operand_Type      := (Register_Mode, False, 0);
         Offset       : Word_8            := 0;
         FAC          : FP_Register_Index := 0;
         VAC_1, VAC_2 : V_Register_Index  := 0;
         F_Operand    : Operand_Type      := (Register_Mode, False, 0);
         V_Operand    : Operand_Type      := (Register_Mode, False, 0);
      end record;

   function Decode
     (IR : Word_16)
      return Instruction_Record;

   function Encode
     (Rec : Instruction_Record)
      return Word_16;

   function Timing
     (Rec : Instruction_Record)
      return Microsecond_Duration;

end Pdp11.ISA;
