package body Pdp11.ISA is

   function Bits
     (From   : Word_16;
      Lo, Hi : Natural)
      return Word_16
   is (From / 2 ** Lo mod 2 ** (Hi - Lo + 1));

   function Operand
     (From : Word_16;
      Lo   : Natural)
      return Operand_Type
   is (Mode_Type'Val (Bits (From, Lo + 4, Lo + 5)),
       Bits (From, Lo + 3, Lo + 3) = 1,
       Register_Index (Bits (From, Lo, Lo + 2)));

   function Get_Instruction
     (IR     : Word_16;
      Base   : Instruction_Type;
      Lo, Hi : Natural)
      return Instruction_Type
   is (Instruction_Type'Val
       (Instruction_Type'Pos (Base)
          + Bits (IR, Lo, Hi)));

   function Get_Opcode
     (Instruction : Instruction_Type;
      Base        : Instruction_Type;
      Offset      : Natural)
      return Word_16
   is ((Instruction_Type'Pos (Instruction)
       - Instruction_Type'Pos (Base))
       * 2 ** Offset);

   To_Dbl_Op : constant array (Word_16 range 1 .. 6) of Instruction_Type :=
                 (1 => I_MOV,
                  2 => I_CMP,
                  3 => I_BIT,
                  4 => I_BIC,
                  5 => I_BIS,
                  6 => I_ADD);

   To_Sng_Op : constant array (Word_16 range 0 .. 15) of Instruction_Type :=
                 (0 => I_CLR,
                  1 => I_COM,
                  2 => I_INC,
                  3 => I_DEC,
                  4 => I_NEG,
                  5 => I_ADC,
                  6 => I_SBC,
                  7 => I_TST,
                  8 => I_ROR,
                  9 => I_ROL,
                  10 => I_ASR,
                  11 => I_ASL,
                  12 => I_MARK,
                  13 => I_MFPI,
                  14 => I_MTPI,
                  15 => I_SXT);

   To_Br_Op            : constant array (Boolean, Word_16 range 0 .. 7)
     of Instruction_Type
       := (False =>
               (0 => I_BR,
                1 => I_BEQ,
                2 => I_BLT,
                3 => I_BLE,
                4 => I_BMI,
                5 => I_BLOS,
                6 => I_BVS,
                7 => I_BCS),
           True  =>
               (0 => I_BR,
                1 => I_BNE,
                2 => I_BGE,
                3 => I_BGT,
                4 => I_BPL,
                5 => I_BHI,
                6 => I_BVC,
                7 => I_BCC));

   function To_Floating_Point_Format_1
     (Opcode : Word_16;
      AC     : Word_16;
      Src    : Word_16)
      return Instruction_Record;

   ------------------
   -- Basic_Timing --
   ------------------

   function Basic_Timing
     (Instruction : Instruction_Type)
      return Microsecond_Duration
   is
   begin
      case Instruction is
         when I_ADD | I_SUB | I_BIC | I_BIS =>
            return 3.17;
         when I_CMP | I_BIT =>
            return 2.91;
         when I_MOV =>
            return 2.91;
         when I_CLR | I_COM | I_INC | I_DEC | I_NEG | I_ADC | I_SBC =>
            return 2.65;
         when I_ROR | I_ROL | I_ASR | I_ASL =>
            return 2.91;
         when I_TST =>
            return 2.39;
         when Branch_Instruction =>
            return 1.87;
         when Floating_Point_F1 =>
            return 6.0;
         when Floating_Point_F2 =>
            return 5.0;
         when others =>
            return 3.1;
      end case;
   end Basic_Timing;

   ------------
   -- Decode --
   ------------

   function Decode (IR : Word_16) return Instruction_Record is
      Word_Op    : constant Boolean := Bits (IR, 15, 15) = 0;
      Dbl_Opcode : constant Word_16 := Bits (IR, 12, 14);
      Sng_Opcode : constant Word_16 := Bits (IR, 6, 14);
      Br_Opcode  : constant Word_16 :=
                     Bits (IR, 15, 15) * 4 + Bits (IR, 9, 10);
      Br_Negate  : constant Boolean := Bits (IR, 8, 8) = 0;
      Rec        : Instruction_Record;
   begin
      if Dbl_Opcode in To_Dbl_Op'Range then
         Rec.Instruction := To_Dbl_Op (Dbl_Opcode);
         Rec.Word := Word_Op;
         if Rec.Instruction = I_ADD and then not Word_Op then
            Rec.Instruction := I_SUB;
            Rec.Word := True;
         end if;
         Rec.Src := Operand (IR, 6);
         Rec.Dst := Operand (IR, 0);
      elsif Sng_Opcode in 8#50# .. 8#63# then
         Rec.Instruction := To_Sng_Op (Sng_Opcode - 40);
         Rec.Word := Word_Op;
         Rec.Dst := Operand (IR, 0);
      elsif Sng_Opcode in 8#64# .. 8#67#
        and then (Bits (IR, 15, 15) = 1
                  or else Sng_Opcode > 8#64#)
      then
         if Bits (IR, 15, 15) = 0 then
            Rec.Instruction := Get_Instruction (Sng_Opcode - 8#64#,
                                                I_MARK, 0, 1);
         else
            Rec.Instruction := Get_Instruction (Sng_Opcode - 8#64#,
                                                I_MTPS, 0, 1);
         end if;
         Rec.Dst := Operand (IR, 0);
      elsif IR in 8#077000# .. 8#077777# then
         Rec.Instruction := I_SOB;
         Rec.Src.Register := Register_Index (Bits (IR, 6, 8));
         Rec.Offset := Word_8 (Bits (IR, 0, 5));
      elsif Bits (IR, 6, 15) = 1 then
         Rec.Instruction := I_JMP;
         Rec.Dst := Operand (IR, 0);
      elsif Bits (IR, 9, 15) = 4 then
         Rec.Instruction := I_JSR;
         Rec.Dst := Operand (IR, 0);
         Rec.Src.Register := Register_Index (Bits (IR, 6, 8));
      elsif IR in 8#000240# .. 8#000277# then
         Rec.Instruction := (if Bits (IR, 4, 4) = 0 then I_CCC else I_SCC);
         Rec.N := Bits (IR, 3, 3) = 1;
         Rec.Z := Bits (IR, 2, 2) = 1;
         Rec.V := Bits (IR, 1, 1) = 1;
         Rec.C := Bits (IR, 0, 0) = 1;
      elsif Bits (IR, 3, 15) = 8#00020# then
         Rec.Instruction := I_RTS;
         Rec.Src.Register := Register_Index (Bits (IR, 0, 2));
      elsif Bits (IR, 11, 14) = 0
        and then IR not in 8#000210# .. 8#000227#  --  BR, negated
      then
         Rec.Instruction := To_Br_Op (Br_Negate, Br_Opcode);
         Rec.Offset := Word_8 (IR mod 256);
      elsif Bits (IR, 9, 15) in 8#171# .. 8#174# then
         Rec :=
           To_Floating_Point_Format_1
             (Bits (IR, 8, 11), Bits (IR, 6, 7), Bits (IR, 0, 5));
      elsif IR in 8#170400# .. 8#170777# then
         Rec := Instruction_Record'
           (Instruction => Get_Instruction (IR, I_CLRF, 6, 7),
            F_Operand   => Operand (IR, 0),
            others      => <>);
      elsif IR in 8#170300# .. 8#170377# then
         Rec := Instruction_Record'
           (Instruction => I_INVF,
            F_Operand   => Operand (IR, 0),
            others      => <>);
      elsif IR in 8#007000# .. 8#007777# then
         Rec := Instruction_Record'
           (Instruction => Get_Instruction (IR, I_LDV, 8, 8),
            VAC_1       => V_Register_Index (Bits (IR, 6, 7)),
            V_Operand   => Operand (IR, 0),
            others      => <>);
      elsif IR in 8#107000# .. 8#107077# then
         Rec := Instruction_Record'
           (Instruction => Get_Instruction (IR, I_ADDV, 4, 5),
            VAC_1       => V_Register_Index (Bits (IR, 2, 3)),
            VAC_2       => V_Register_Index (Bits (IR, 0, 1)),
            others      => <>);
      elsif IR in 8#000210# .. 8#000223# then
         Rec := Instruction_Record'
           (Instruction => Get_Instruction (IR - 8#210#, I_CLRV, 2, 3),
            VAC_1       => V_Register_Index (Bits (IR, 0, 1)),
            others      => <>);
      else
         Rec := (Undefined => True, others => <>);
      end if;
      return Rec;
   end Decode;

   ------------------------
   -- Dst_Operand_Timing --
   ------------------------

   function Dst_Operand_Timing
     (Instruction : Instruction_Type;
      Operand     : Operand_Type)
      return Microsecond_Duration
   is
      Reduction : constant Microsecond_Duration :=
                    (if Instruction in I_CMP | I_TST | I_BIT
                     and then (Operand.Mode /= Register_Mode
                       or else Operand.Deferred)
                     then 0.4
                     else 0.0);
      Timing    : constant Microsecond_Duration :=
                    (case Operand.Mode is
                        when Register_Mode =>
                       (if Operand.Deferred then 1.48 else 0.0),
                        when Autoincrement_Mode =>
                       (if Operand.Deferred then 3.20 else 1.76),
                        when Autodecrement_Mode =>
                       (if Operand.Deferred then 3.20 else 1.76),
                        when Index_Mode         =>
                       (if Operand.Deferred then 4.92 else 3.46));
   begin
      return Timing - Reduction;
   end Dst_Operand_Timing;

   ------------
   -- Encode --
   ------------

   function Encode
     (Operand : Operand_Type;
      Offset  : Natural)
      return Word_16
   is
      Code : constant Word_16 :=
               Word_16 (Operand.Register)
               + 8 * Boolean'Pos (Operand.Deferred)
               + 16 * Mode_Type'Pos (Operand.Mode);
   begin
      return Code * 2 ** Offset;
   end Encode;

   ------------
   -- Encode --
   ------------

   function Encode
     (Rec : Instruction_Record)
      return Word_16
   is
      function Dbl (Opcode : Word_16) return Word_16
      is (Boolean'Pos (not Rec.Word) * 2 ** 15
          + Opcode * 2 ** 12
          + Encode (Rec.Src, 6)
          + Encode (Rec.Dst, 0));

      function Dbl_Reg (Opcode : Word_16) return Word_16
      is (8#070000#
          + Opcode * 2 ** 9
          + Word_16 (Rec.Src.Register) * 2 ** 6
          + Encode (Rec.Dst, 0));

      function Sng (Opcode : Word_16) return Word_16
      is (Boolean'Pos (not Rec.Word) * 2 ** 15
          + 2 ** 11
          + Opcode * 2 ** 6
          + Encode (Rec.Dst, 0));

      function Br (Opcode : Word_16;
                   Negated : Boolean)
                   return Word_16
      is ((Opcode / 4) * 2 ** 15
          + (Opcode mod 4) * 2 ** 9
          + (if Negated then 2 ** 8 else 0)
          + Word_16 (Rec.Offset));

   begin
      case Rec.Instruction is
         when Double_Operand_Instruction =>
            if Rec.Instruction = I_SUB then
               return 8#160000#
               + Encode (Rec.Src, 6)
                 + Encode (Rec.Dst, 0);
            else
               return Dbl
                 (Instruction_Type'Pos (Rec.Instruction)
                  - Instruction_Type'Pos (Double_Operand_Instruction'First)
                  + 1);
            end if;
         when Sized_Single_Operand_Instruction =>
            return Sng
              (Instruction_Type'Pos (Rec.Instruction)
               - Instruction_Type'Pos (Sized_Single_Operand_Instruction'First)
               + 8);
         when Unsized_Single_Operand_Instruction =>

            declare
               Opcode : constant Word_16 :=
                 Get_Opcode (Rec.Instruction,
                             Unsized_Single_Operand_Instruction'First,
                             Offset => 0);
            begin
               return 8#006400#
                 + Bits (Opcode, 2, 2) * 2 ** 15
                 + Bits (Opcode, 0, 1) * 2 ** 6
                 + Encode (Rec.Dst, 0);
            end;
         when Register_Double_Operand_Instruction =>
            return Dbl_Reg
              (Instruction_Type'Pos (Rec.Instruction)
               - Instruction_Type'Pos
                 (Register_Double_Operand_Instruction'First));
         when Branch_Instruction =>
            if Rec.Instruction >= I_BNE then
               return Br
                 (Instruction_Type'Pos (Rec.Instruction)
                  - Instruction_Type'Pos (I_BNE)
                  + 1,
                  True);
            else
               return Br
                 (Instruction_Type'Pos (Rec.Instruction)
                  - Instruction_Type'Pos (Branch_Instruction'First),
                  False);
            end if;
         when I_SOB =>
            return 8#077000#
            + Word_16 (Rec.Src.Register) * 64
              + Word_16 (Rec.Offset);
         when I_JMP =>
            return 8#000100# + Encode (Rec.Dst, 0);
         when I_JSR =>
            return 8#004000#
            + Word_16 (Rec.Src.Register) * 2 ** 6
              + Encode (Rec.Dst, 0);
         when I_RTS =>
            return 8#000200# + Word_16 (Rec.Src.Register);

         when I_CCC | I_SCC =>
            return 8#000240#
            + (if Rec.Instruction = I_SCC then 16 else 0)
            + Boolean'Pos (Rec.N) * 8
              + Boolean'Pos (Rec.Z) * 4
              + Boolean'Pos (Rec.V) * 2
              + Boolean'Pos (Rec.C);
         when Floating_Point_F1 =>
            return 8#171000#
            + (Instruction_Type'Pos (Rec.Instruction)
               - Instruction_Type'Pos (Floating_Point_F1'First))
              * 256
              + Word_16 (Rec.FAC) * 64
              + Encode (Rec.F_Operand, 0);
         when Floating_Point_F2 =>
            return 8#170400#
            + (Instruction_Type'Pos (Rec.Instruction)
               - Instruction_Type'Pos (Floating_Point_F2'First))
              * 64
              + Encode (Rec.F_Operand,  0);
         when I_INVF =>
            return 8#170300# + Encode (Rec.F_Operand, 0);
         when Vector_F1 =>
            return 8#007000#
            + Get_Opcode (Rec.Instruction, Vector_F1'First, 8)
              + Word_16 (Rec.VAC_1) * 2 ** 6
              + Encode (Rec.V_Operand, 0);
         when Vector_F2 =>
            return 8#107000#
            + Get_Opcode (Rec.Instruction, Vector_F2'First, 4)
              + Word_16 (Rec.VAC_1) * 4
              + Word_16 (Rec.VAC_2);
         when Vector_F3 =>
            return 8#000210#
            + Get_Opcode (Rec.Instruction, Vector_F3'First, 2)
              + Word_16 (Rec.VAC_1);
      end case;
   end Encode;

   ------------------------
   -- Src_Operand_Timing --
   ------------------------

   function Src_Operand_Timing
     (Instruction : Instruction_Type;
      Operand     : Operand_Type)
      return Microsecond_Duration
   is
      pragma Unreferenced (Instruction);
   begin
      case Operand.Mode is
         when Register_Mode =>
            return (if Operand.Deferred then 0.94 else 0.0);
         when Autoincrement_Mode =>
            return (if Operand.Deferred then 2.66 else 1.20);
         when Autodecrement_Mode =>
            return (if Operand.Deferred then 2.66 else 1.20);
         when Index_Mode =>
            return (if Operand.Deferred then 4.38 else 2.92);
      end case;
   end Src_Operand_Timing;

   --------------------------------
   -- To_Floating_Point_Format_1 --
   --------------------------------

   function To_Floating_Point_Format_1
     (Opcode : Word_16;
      AC     : Word_16;
      Src    : Word_16)
      return Instruction_Record
   is
   begin
      return Instruction_Record'
        (Instruction =>
           Instruction_Type'Val
             (Floating_Point_F1'Pos (Floating_Point_F1'First)
              + Opcode - 2),
         FAC         => FP_Register_Index (AC),
         F_Operand   => Operand (Src, 0),
         others      => <>);
   end To_Floating_Point_Format_1;

end Pdp11.ISA;
