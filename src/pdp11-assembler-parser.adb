with Ada.Characters.Handling;

with GCS.Constraints;

with Pdp11.Tokens;                   use Pdp11.Tokens;
with Pdp11.Lexical;                  use Pdp11.Lexical;

with Pdp11.ISA;
with Pdp11.Options;

package body Pdp11.Assembler.Parser is

   function At_Operand return Boolean;
   function Parse_Operand
     (Assembly    : in out Assembly_Type'Class;
      Instruction : Pdp11.ISA.Instruction_Type)
      return Pdp11.ISA.Operand_Type;

   function At_Register return Boolean;
   function Parse_Register
     return Pdp11.ISA.Register_Index
     with Pre => At_Register;

   function At_FAC return Boolean;
   function Parse_FAC
     return Pdp11.ISA.FP_Register_Index
     with Pre => At_FAC;

   function At_VAC return Boolean;
   function Parse_VAC
     return Pdp11.ISA.V_Register_Index
     with Pre => At_VAC;

   function At_Branch_Operand return Boolean;
   procedure Parse_Branch_Operand
     (Assembly : in out Assembly_Type'Class);

   procedure Parse_Values
     (Assembly  : in out Assembly_Type'Class;
      Word_Size : Boolean);

   procedure Parse_Constant (Assembly : in out Assembly_Type'Class);
   procedure Parse_Directive (Assembly : in out Assembly_Type'Class);
   procedure Parse_Instruction (Assembly : in out Assembly_Type'Class);
   procedure Parse_Label (Assembly : in out Assembly_Type'Class);

   function At_Expression return Boolean
   is (Tok = Tok_Plus or else Tok = Tok_Minus
       or else Tok = Tok_Integer_Constant or else Tok = Tok_Identifier);

   function Parse_Static_Expression
     (Assembly : Assembly_Type'Class)
      return Word_16;

   procedure Skip_Line;

   package Instruction_Maps is
     new WL.String_Maps
       (Pdp11.ISA.Instruction_Record,
        Pdp11.ISA."=");

   Name_Table : Instruction_Maps.Map;

   function Get_Instruction_Info
     (Name : String)
      return Pdp11.ISA.Instruction_Record;

   procedure Check_Name_Table;

   type Directive_Parser is access
     procedure (Assembly : in out Assembly_Type'Class);

   package Directive_Parser_Maps is
     new WL.String_Maps (Directive_Parser);

   Directive_Parser_Table : Directive_Parser_Maps.Map;

   procedure Check_Directive_Table;

   procedure Parse_Asciz (Assembly : in out Assembly_Type'Class);
   procedure Parse_Byte (Assembly : in out Assembly_Type'Class);
   procedure Parse_Even (Assembly : in out Assembly_Type'Class);
   procedure Parse_Word (Assembly : in out Assembly_Type'Class);
   procedure Parse_Zero (Assembly : in out Assembly_Type'Class);

   -----------------------
   -- At_Branch_Operand --
   -----------------------

   function At_Branch_Operand return Boolean is
   begin
      return not At_Register
        and then (Tok = Tok_Identifier
                  or else Tok = Tok_Integer_Constant);
   end At_Branch_Operand;

   ------------
   -- At_FAC --
   ------------

   function At_FAC return Boolean is
      Img : constant String :=
              Ada.Characters.Handling.To_Lower (Tok_Text);
   begin
      if Tok /= Tok_Identifier
        or else Img'Length /= 3
      then
         return False;
      end if;

      return (Img (Img'First .. Img'First + 1) = "ac"
              and then Img (Img'Last) in '0' .. '5');
   end At_FAC;

   ----------------
   -- At_Operand --
   ----------------

   function At_Operand return Boolean is
   begin
      return Tok in Tok_Hash | Tok_Ampersand | Tok_Left_Paren
        | Tok_Minus | Tok_Identifier | Tok_Integer_Constant;
   end At_Operand;

   -----------------
   -- At_Register --
   -----------------

   function At_Register return Boolean is
      Img : constant String :=
              Ada.Characters.Handling.To_Lower (Tok_Text);
   begin
      if Tok /= Tok_Identifier
        or else Img'Length /= 2
      then
         return False;
      end if;

      return (Img (Img'First) = 'r' and then Img (Img'Last) in '0' .. '7')
        or else Img = "sp" or else Img = "pc";
   end At_Register;

   ------------
   -- At_VAC --
   ------------

   function At_VAC return Boolean is
      Img : constant String :=
              Ada.Characters.Handling.To_Lower (Tok_Text);
   begin
      if Tok /= Tok_Identifier
        or else Img'Length /= 2
      then
         return False;
      end if;

      return (Img (Img'First .. Img'First) = "v"
              and then Img (Img'Last) in '0' .. '3');
   end At_VAC;

   ---------------------------
   -- Check_Directive_Table --
   ---------------------------

   procedure Check_Directive_Table is

      procedure Add
        (Name : String;
         Parser : Directive_Parser);

      ---------
      -- Add --
      ---------

      procedure Add
        (Name   : String;
         Parser : Directive_Parser)
      is
      begin
         Directive_Parser_Table.Insert (Name, Parser);
      end Add;

   begin
      if not Directive_Parser_Table.Is_Empty then
         return;
      end if;

      Add ("asciz", Parse_Asciz'Access);
      Add ("byte", Parse_Byte'Access);
      Add ("even", Parse_Even'Access);
      Add ("word", Parse_Word'Access);
      Add ("zero", Parse_Zero'Access);
   end Check_Directive_Table;

   ----------------------
   -- Check_Name_Table --
   ----------------------

   procedure Check_Name_Table is

      use Pdp11.ISA;

      procedure Operand
        (Name        : String;
         Instruction : Instruction_Type;
         Word        : Boolean);

      procedure Branch
        (Name        : String;
         Instruction : Instruction_Type;
         Negate      : Boolean);

      procedure Flags
        (Name       : String;
         Set        : Boolean;
         N, Z, C, V : Boolean := False);

      procedure Floating_Point
        (Name        : String;
         Instruction : Instruction_Type);

      procedure Vector
        (Name        : String;
         Instruction : Instruction_Type);

      ------------
      -- Branch --
      ------------

      procedure Branch
        (Name        : String;
         Instruction : Instruction_Type;
         Negate      : Boolean)
      is
      begin
         Name_Table.Insert
           (Name,
            Instruction_Record'
              (Instruction => Instruction,
               Negate      => Negate,
               Offset      => 0,
               others      => <>));
      end Branch;

      -----------
      -- Flags --
      -----------

      procedure Flags
        (Name       : String;
         Set        : Boolean;
         N, Z, C, V : Boolean := False)
      is
      begin
         Name_Table.Insert
           (Name,
            Instruction_Record'
              (Instruction => (if Set then I_SCC else I_CCC),
               N           => N,
               Z           => Z,
               V           => V,
               C           => C,
               others      => <>));
      end Flags;

      --------------------
      -- Floating_Point --
      --------------------

      procedure Floating_Point
        (Name        : String;
         Instruction : Instruction_Type)
      is
      begin
         Name_Table.Insert
           (Name,
            Instruction_Record'
              (Instruction => Instruction,
               others      => <>));
      end Floating_Point;

      -------------
      -- Operand --
      -------------

      procedure Operand
        (Name        : String;
         Instruction : Instruction_Type;
         Word        : Boolean)
      is
      begin
         Name_Table.Insert
           (Name,
            Instruction_Record'
              (Instruction => Instruction,
               Word        => Word,
               others      => <>));
      end Operand;

      ------------
      -- Vector --
      ------------

      procedure Vector
        (Name        : String;
         Instruction : Instruction_Type)
      is
      begin
         Name_Table.Insert
           (Name,
            Instruction_Record'
              (Instruction => Instruction,
               others      => <>));
      end Vector;

   begin
      if not Name_Table.Is_Empty then
         return;
      end if;

      Operand ("mov", I_MOV, True);
      Operand ("movb", I_MOV, False);
      Operand ("cmp", I_CMP, True);
      Operand ("cmpb", I_CMP, False);
      Operand ("bit", I_BIT, True);
      Operand ("bitb", I_BIT, False);
      Operand ("bic", I_BIC, True);
      Operand ("bicb", I_BIC, False);
      Operand ("bis", I_BIS, True);
      Operand ("bisb", I_BIS, False);
      Operand ("add", I_ADD, True);
      Operand ("sub", I_SUB, True);

      Operand ("mul", I_MUL, True);
      Operand ("div", I_DIV, True);
      Operand ("ash", I_ASH, True);
      Operand ("ashc", I_ASHC, True);
      Operand ("xor", I_XOR, True);
      Operand ("sob", I_SOB, True);

      Operand ("clr", I_CLR, True);
      Operand ("clrb", I_CLR, False);
      Operand ("com", I_COM, True);
      Operand ("comb", I_COM, False);
      Operand ("inc", I_INC, True);
      Operand ("incb", I_INC, False);
      Operand ("dec", I_DEC, True);
      Operand ("decb", I_DEC, False);
      Operand ("neg", I_NEG, True);
      Operand ("negb", I_NEG, False);
      Operand ("adc", I_ADC, True);
      Operand ("adcb", I_ADC, False);
      Operand ("sbc", I_SBC, True);
      Operand ("sbcb", I_SBC, False);
      Operand ("tst", I_TST, True);
      Operand ("tstb", I_TST, False);
      Operand ("ror", I_ROR, True);
      Operand ("rorb", I_ROR, False);
      Operand ("rol", I_ROL, True);
      Operand ("rolb", I_ROL, False);
      Operand ("asr", I_ASR, True);
      Operand ("asrb", I_ASR, False);
      Operand ("asl", I_ASL, True);
      Operand ("aslb", I_ASL, False);

      Branch ("br", I_BR, False);
      Branch ("bne", I_BEQ, True);
      Branch ("beq", I_BEQ, False);
      Branch ("bge", I_BLT, True);
      Branch ("blt", I_BLT, False);
      Branch ("bgt", I_BLE, True);
      Branch ("ble", I_BLE, False);
      Branch ("bpl", I_BMI, True);
      Branch ("bmi", I_BMI, False);
      Branch ("bhi", I_BLOS, True);
      Branch ("blos", I_BLOS, False);
      Branch ("bvc", I_BVS, True);
      Branch ("bvs", I_BVS, False);
      Branch ("bcc", I_BCS, True);
      Branch ("bcs", I_BCS, False);
      Branch ("bhis", I_BCS, True);
      Branch ("blo", I_BCS, True);

      Flags ("sen", True, N => True);
      Flags ("sez", True, Z => True);
      Flags ("sec", True, C => True);
      Flags ("sev", True, V => True);

      Flags ("cln", False, N => True);
      Flags ("clz", False, Z => True);
      Flags ("clc", False, C => True);
      Flags ("clv", False, V => True);

      Flags ("scc", True, True, True, True, True);
      Flags ("ccc", False, True, True, True, True);

      Name_Table.Insert
        ("jmp",
         (Instruction => I_JMP, others => <>));
      Name_Table.Insert
        ("jsr",
         (Instruction => I_JSR, others => <>));
      Name_Table.Insert
        ("rts",
         (Instruction => I_RTS, others => <>));

      Floating_Point ("mulf", I_MULF);
      Floating_Point ("modf", I_MODF);
      Floating_Point ("addf", I_ADDF);
      Floating_Point ("ldf", I_LDF);
      Floating_Point ("subf", I_SUBF);
      Floating_Point ("cmpf", I_CMPF);
      Floating_Point ("stf", I_STF);
      Floating_Point ("divf", I_DIVF);

      Floating_Point ("clrf", I_CLRF);
      Floating_Point ("tstf", I_TSTF);
      Floating_Point ("absf", I_ABSF);
      Floating_Point ("negf", I_NEGF);

      Vector ("ldv", I_LDV);
      Vector ("stv", I_STV);
      Vector ("addv", I_ADDV);
      Vector ("subv", I_SUBV);
      Vector ("absv", I_ABSV);
      Vector ("mulfv", I_MULFV);
      Vector ("clrv", I_CLRV);
      Vector ("negv", I_NEGV);
      Vector ("normv", I_NORMV);

   end Check_Name_Table;

   --------------------------
   -- Get_Instruction_Info --
   --------------------------

   function Get_Instruction_Info
     (Name : String)
      return Pdp11.ISA.Instruction_Record
   is
   begin
      Check_Name_Table;
      if Name_Table.Contains (Name) then
         return Name_Table.Element (Name);
      else
         Error (Name & ": unknown instruction");
         return Pdp11.ISA.Instruction_Record'(others => <>);
      end if;
   end Get_Instruction_Info;

   -----------------
   -- Parse_Asciz --
   -----------------

   procedure Parse_Asciz (Assembly : in out Assembly_Type'Class) is
   begin
      if Tok = Tok_String_Constant then
         declare
            Text : constant String := Tok_Text;
         begin
            for Ch of Text loop
               Assembly.Append_Byte (Character'Pos (Ch));
            end loop;
         end;

         Assembly.Append_Byte (0);
         Scan;

      else
         Error ("expected a string");
         Skip_Line;
      end if;
   end Parse_Asciz;

   --------------------------
   -- Parse_Branch_Operand --
   --------------------------

   procedure Parse_Branch_Operand
     (Assembly : in out Assembly_Type'Class)
   is
   begin
      if Tok = Tok_Identifier then
         Assembly.Reference
           (Name => Tok_Text,
            Ref  => Branch);
         Scan;
      elsif Tok = Tok_Integer_Constant then
         Assembly.Reference
           (Label => Natural'Value (Tok_Text),
            Ref   => Branch);
         Scan;
      else
         raise Constraint_Error with
           "expected a branch label, but found "
           & Tok'Image & " " & Tok_Text;
      end if;
   end Parse_Branch_Operand;

   -----------------
   -- Parse_Bytes --
   -----------------

   procedure Parse_Byte (Assembly : in out Assembly_Type'Class) is
   begin
      Parse_Values (Assembly, False);
   end Parse_Byte;

   --------------------
   -- Parse_Constant --
   --------------------

   procedure Parse_Constant (Assembly : in out Assembly_Type'Class) is
      pragma Assert (Tok = Tok_Identifier);
      Name : constant String := Tok_Text;
   begin
      Scan;
      pragma Assert (Tok = Tok_Equal);
      Scan;
      Assembly.Define_Constant (Name, Parse_Static_Expression (Assembly));
   end Parse_Constant;

   ---------------------
   -- Parse_Directive --
   ---------------------

   procedure Parse_Directive (Assembly : in out Assembly_Type'Class) is
      pragma Assert (Tok = Tok_Identifier);
      Name : constant String := Tok_Text;
   begin

      Check_Directive_Table;

      if Directive_Parser_Table.Contains (Name) then
         Scan;
         Directive_Parser_Table.Element (Name) (Assembly);
      else
         if Pdp11.Options.Warnings then
            Warning ("unknown directive: " & Name);
         end if;
         Skip_Line;
      end if;
   end Parse_Directive;

   ----------------
   -- Parse_Even --
   ----------------

   procedure Parse_Even (Assembly : in out Assembly_Type'Class) is
   begin
      if Assembly.Current mod 2 = 1 then
         Assembly.Append_Byte (0);
      end if;
   end Parse_Even;

   ---------------
   -- Parse_FAC --
   ---------------

   function Parse_FAC
     return Pdp11.ISA.FP_Register_Index
   is
      Name : constant String :=
               Ada.Characters.Handling.To_Lower (Tok_Text);
      Index : constant Natural :=
                Character'Pos (Name (Name'Last)) - Character'Pos ('0');
   begin
      Scan;
      return Pdp11.ISA.FP_Register_Index (Index);
   end Parse_FAC;

   -----------------------
   -- Parse_Instruction --
   -----------------------

   procedure Parse_Instruction (Assembly : in out Assembly_Type'Class) is
      use Pdp11.ISA;

      pragma Assert (Tok = Tok_Identifier);
      Mnemonic : constant String := Tok_Text;
      Rec      : Pdp11.ISA.Instruction_Record :=
                   Get_Instruction_Info (Mnemonic);
      Address  : constant Word_16 := Assembly.Current;
   begin
      Scan;
      Assembly.Append (0);

      if Has_Branch_Operand (Rec.Instruction) then
         if At_Branch_Operand then
            Parse_Branch_Operand (Assembly);
         else
            Error ("missing branch label");
         end if;
      elsif Has_Source_Register (Rec.Instruction) then
         if At_Register then
            Rec.Src.Register := Parse_Register;
         else
            Error ("missing register operand");
         end if;
      elsif Has_Source_Operand (Rec.Instruction) then
         if At_Operand then
            Rec.Src := Parse_Operand (Assembly, Rec.Instruction);
         else
            Error ("missing source operand");
         end if;
      elsif Rec.Instruction in Floating_Point_F1 then
         if At_FAC then
            Rec.FAC := Parse_FAC;
         else
            Error ("missing AC");
         end if;
         if Tok = Tok_Comma then
            Scan;
         else
            Error ("missing ','");
         end if;
         if not At_Operand then
            Error ("missing operand");
         else
            Rec.F_Operand := Parse_Operand (Assembly, Rec.Instruction);
         end if;
      elsif Rec.Instruction in Floating_Point_F2
        or else Rec.Instruction = I_INVF
      then
         if At_FAC then
            Rec.FAC := Parse_FAC;
            Rec.F_Operand :=
              Operand_Type'
                (Mode     => Register_Mode,
                 Deferred => False,
                 Register => Register_Index (Rec.FAC));
         elsif At_Operand then
            Rec.F_Operand := Parse_Operand (Assembly, Rec.Instruction);
         else
            Error ("expected an operand");
         end if;
      elsif Rec.Instruction in Vector_F1 then
         if At_VAC then
            Rec.VAC_1 := Parse_VAC;
            if Tok = Tok_Comma then
               Scan;
            else
               Error ((if Rec.Instruction = I_LDV
                      then "source" else "destination")
                      & " operand required");
            end if;
            if At_Operand then
               Rec.V_Operand :=
                 Parse_Operand (Assembly, Rec.Instruction);
            else
               Error ("missing operand");
            end if;
         else
            Error ("missing vector accumulator");
         end if;
      elsif Rec.Instruction in Vector_F2 then
         if At_VAC then
            if Rec.Instruction = I_MULFV then
               Error ("floating point register required");
            end if;
            Rec.VAC_1 := Parse_VAC;
         elsif At_FAC then
            if Rec.Instruction = I_MULFV then
               Rec.FAC := Parse_FAC;
               if Rec.FAC > 3 then
                  Error ("only ac0 - ac3 can be used with mulfv");
               else
                  Rec.VAC_1 := V_Register_Index (Rec.FAC);
               end if;
            else
               Error ("vector register required");
            end if;
         else
            Error ("missing source register");
         end if;

         if Tok = Tok_Comma then
            Scan;
            if At_VAC then
               if Rec.Instruction = I_ABSV then
                  Error ("floating point register required");
               end if;
               Rec.VAC_2 := Parse_VAC;
            elsif At_FAC then
               if Rec.Instruction /= I_ABSV then
                  Error ("vector register required");
               end if;
               Rec.FAC := Parse_FAC;
               if Rec.FAC > 3 then
                  Error ("only ac0 .. ac3 can be used with absv");
               end if;
            else
               Error ("missing destination register");
            end if;
         else
            Error ("missing destination");
         end if;

      elsif Rec.Instruction in Vector_F3 then
         if At_VAC then
            Rec.VAC_1 := Parse_VAC;
         else
            Error ("missing register");
         end if;
      end if;

      if Has_Destination_Operand (Rec.Instruction) then
         if Has_Source_Operand (Rec.Instruction)
           or else Has_Source_Register (Rec.Instruction)
         then
            if Tok = Tok_Comma then
               Scan;
               if not At_Operand then
                  Error ("missing operand");
               end if;
            elsif At_Operand then
               Error ("missing ','");
            else
               Error ("missing destination");
            end if;
         elsif not At_Operand then
            Error ("missing operand");
         end if;

         if At_Operand then
            Rec.Dst :=
              Parse_Operand (Assembly, Rec.Instruction);
         end if;
      end if;

      Assembly.Set (Address, Encode (Rec));

   end Parse_Instruction;

   -----------------
   -- Parse_Label --
   -----------------

   procedure Parse_Label (Assembly : in out Assembly_Type'Class) is
   begin
      pragma Assert (Tok = Tok_Integer_Constant
                     or else Tok = Tok_Identifier);
      if Tok = Tok_Integer_Constant then
         Assembly.Label (Integer'Value (Tok_Text));
      else
         Assembly.Label (Tok_Text);
      end if;

      Scan;
      pragma Assert (Tok = Tok_Colon);
      Scan;
   end Parse_Label;

   ----------------
   -- Parse_Line --
   ----------------

   procedure Parse_Line (Assembly : in out Assembly_Type'Class) is
      --  use type GCS.Constraints.Line_Number;
      Line : constant GCS.Constraints.Line_Number :=
               Tok_Line;
   begin
      if Tok = Tok_Identifier and then Next_Tok = Tok_Equal then
         Parse_Constant (Assembly);
      elsif Tok = Tok_Dot and then Next_Tok = Tok_Equal then
         Scan;
         Scan;
         Assembly.Current := Parse_Static_Expression (Assembly);
      elsif Tok = Tok_Dot and then Next_Tok = Tok_Identifier then
         Scan;
         Parse_Directive (Assembly);
      else
         while (Tok = Tok_Identifier or else Tok = Tok_Integer_Constant)
           and then Next_Tok = Tok_Colon
         loop
            Parse_Label (Assembly);
         end loop;

         if Tok_Line = Line
           and then Tok = Tok_Identifier
         then
            Parse_Instruction (Assembly);
         end if;

         if Tok_Line = Line then
            Error ("expected end of line");
            Skip_Line;
         end if;

      end if;

   end Parse_Line;

   -------------------
   -- Parse_Operand --
   -------------------

   function Parse_Operand
     (Assembly    : in out Assembly_Type'Class;
      Instruction : Pdp11.ISA.Instruction_Type)
      return Pdp11.ISA.Operand_Type
   is
      use Pdp11.ISA;
      Operand     : Operand_Type;
      Have_Value  : Boolean := False;
      Auto_Dec    : Boolean := False;
      Auto_Inc    : Boolean := False;
      Value_Bytes : Positive := 2;
      Size_32     : constant Boolean :=
                      Instruction in Floating_Point_Instruction;

      procedure Scan_Value
        (Ref : Reference_Type);

      ----------------
      -- Scan_Value --
      ----------------

      procedure Scan_Value
        (Ref : Reference_Type)
      is
         Negative : Boolean := False;
      begin
         if Tok = Tok_Plus then
            Scan;
         elsif Tok = Tok_Minus then
            Scan;
            Negative := True;
         end if;

         if Tok = Tok_Integer_Constant then
            if Value_Bytes > 2 then
               declare
                  Value : Word_32 := Word_32'Value (Tok_Text);
               begin
                  if Negative then
                     Value := Word_32'Last - Value + 1;
                  end if;

                  Scan;

                  while Tok = Tok_Plus or else Tok = Tok_Minus loop
                     declare
                        Subtract : constant Boolean := Tok = Tok_Minus;
                     begin
                        Scan;
                        if Tok = Tok_Integer_Constant then
                           Value := (if Subtract
                                     then Value - Word_32'Value (Tok_Text)
                                     else Value + Word_32'Value (Tok_Text));
                           Scan;
                        else
                           Error ("expected an integer constant");
                        end if;
                     end;
                  end loop;

                  Assembly.Append (Word_16 (Value mod 65536));
                  Assembly.Append (Word_16 (Value / 65536));

               end;
            else
               declare
                  Value : Word_16 := Word_16'Value (Tok_Text);
               begin
                  if Negative then
                     Value := 65535 - Value + 1;
                  end if;

                  Scan;

                  while Tok = Tok_Plus or else Tok = Tok_Minus loop
                     declare
                        Subtract : constant Boolean := Tok = Tok_Minus;
                     begin
                        Scan;
                        if Tok = Tok_Integer_Constant then
                           Value := (if Subtract
                                     then Value - Word_16'Value (Tok_Text)
                                     else Value + Word_16'Value (Tok_Text));
                           Scan;
                        else
                           Error ("expected an integer constant");
                        end if;
                     end;
                  end loop;

                  Assembly.Append (Value);
               end;
            end if;

            Have_Value := True;
         elsif Tok = Tok_Identifier then
            Assembly.Reference
              (Name => Tok_Text,
               Ref  => Ref);
            Assembly.Append (0);
            Scan;
            Have_Value := True;
         else
            Error ("missing value");
            if Tok /= Tok_Comma then
               Scan;
            end if;
         end if;
      end Scan_Value;

   begin
      if Tok = Tok_Ampersand then
         Operand.Deferred := True;
         Scan;
      end if;

      if Tok = Tok_Hash then
         Scan;

         Operand.Mode := Autoincrement_Mode;
         Operand.Register := 7;

         if Size_32 and then not Operand.Deferred then
            Value_Bytes := 4;
         end if;

         Scan_Value (Absolute);

      else

         if Tok = Tok_Minus then
            if Next_Tok = Tok_Left_Paren then
               Auto_Dec := True;
               Scan;
            else
               Scan_Value (Absolute);
            end if;
         elsif not At_Register
           and then (Tok = Tok_Identifier
                     or else Tok = Tok_Integer_Constant)
           and then Next_Tok = Tok_Left_Paren
         then
            Scan_Value (Absolute);
         end if;

         if Tok = Tok_Left_Paren then
            Scan;
            if At_Register then
               Operand.Register := Parse_Register;
            else
               Error ("expected a register name");
               if Tok = Tok_Identifier then
                  Scan;
               end if;
            end if;
            if Tok = Tok_Right_Paren then
               Scan;
            else
               Error ("missing ')'");
            end if;
            if Tok = Tok_Plus then
               Auto_Inc := True;
               Scan;
            end if;

            if Have_Value then
               Operand.Mode := Index_Mode;
            elsif Auto_Dec then
               Operand.Mode := Autodecrement_Mode;
            elsif Auto_Inc then
               Operand.Mode := Autoincrement_Mode;
            else
               Operand.Deferred := True;
               Operand.Mode := Register_Mode;
            end if;
         elsif At_Register then
            Operand.Mode := Register_Mode;
            Operand.Register := Parse_Register;
         elsif At_FAC then
            Operand.Mode := Register_Mode;
            Operand.Register := Register_Index (Parse_FAC);
         elsif Tok = Tok_Identifier then
            Scan_Value (Relative);
            Operand.Mode := Index_Mode;
            Operand.Register := 7;
         else
            Error ("unrecognised operand");
         end if;

      end if;

      return Operand;
   end Parse_Operand;

   --------------------
   -- Parse_Register --
   --------------------

   function Parse_Register
     return Pdp11.ISA.Register_Index
   is
      Name : constant String :=
               Ada.Characters.Handling.To_Lower (Tok_Text);
   begin
      Scan;
      if Name = "pc" then
         return 7;
      elsif Name = "sp" then
         return 6;
      else
         declare
            Index : constant Natural :=
                      Character'Pos (Name (Name'Last)) - Character'Pos ('0');
         begin
            return Pdp11.ISA.Register_Index (Index);
         end;
      end if;
   end Parse_Register;

   -----------------------------
   -- Parse_Static_Expression --
   -----------------------------

   function Parse_Static_Expression
     (Assembly : Assembly_Type'Class)
      return Word_16
   is
   begin
      if Tok = Tok_Integer_Constant then
         declare
            Result : constant Word_16 :=
                       Word_16'Value (Tok_Text);
         begin
            Scan;
            return Result;
         end;
      elsif Tok = Tok_Identifier then
         if not Assembly.Definitions.Contains (Tok_Text)
           or else not Assembly.Definitions.Element (Tok_Text).Defined
         then
            Error ("expression is not static");
            return 0;
         else
            declare
               Result : constant Word_16 :=
                          Assembly.Definitions.Element (Tok_Text).Value;
            begin
               Scan;
               return Result;
            end;
         end if;
      elsif Tok = Tok_Minus then
         Scan;
         declare
            X : constant Word_16 := Parse_Static_Expression (Assembly);
         begin
            return (not X) + 1;
         end;
      else
         Error ("expected an expression");
         Skip_Line;
         return 0;
      end if;
   end Parse_Static_Expression;

   ---------------
   -- Parse_VAC --
   ---------------

   function Parse_VAC
     return Pdp11.ISA.V_Register_Index
   is
      Name  : constant String :=
                Ada.Characters.Handling.To_Lower (Tok_Text);
      Index : constant Natural :=
                Character'Pos (Name (Name'Last)) - Character'Pos ('0');
   begin
      Scan;
      return Pdp11.ISA.V_Register_Index (Index);
   end Parse_VAC;

   ------------------
   -- Parse_Values --
   ------------------

   procedure Parse_Values
     (Assembly  : in out Assembly_Type'Class;
      Word_Size : Boolean)
   is
   begin
      while Tok = Tok_Integer_Constant or else Tok = Tok_Identifier loop
         declare
            Value : constant Word_16 := Parse_Static_Expression (Assembly);
         begin
            if Word_Size then
               Assembly.Append (Value);
            else
               Assembly.Append_Byte (Word_8 (Value));
            end if;
            exit when Tok /= Tok_Comma;
            Scan;
         end;
      end loop;
   end Parse_Values;

   ----------------
   -- Parse_Word --
   ----------------

   procedure Parse_Word (Assembly : in out Assembly_Type'Class) is
   begin
      Parse_Values (Assembly, True);
   end Parse_Word;

   ----------------
   -- Parse_Zero --
   ----------------

   procedure Parse_Zero (Assembly : in out Assembly_Type'Class) is
   begin
      if At_Expression then
         for I in 1 .. Parse_Static_Expression (Assembly) loop
            Assembly.Append_Byte (0);
         end loop;
      end if;
   end Parse_Zero;

   ---------------
   -- Skip_Line --
   ---------------

   procedure Skip_Line is
      Line : constant GCS.Constraints.Line_Number := Tok_Line;
   begin
      while Tok_Line = Line loop
         Scan;
      end loop;
   end Skip_Line;

end Pdp11.Assembler.Parser;
