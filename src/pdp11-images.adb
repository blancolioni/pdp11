package body Pdp11.Images is

   function Image (Operand : ISA.Operand_Type) return String;

   ---------------
   -- Hex_Image --
   ---------------

   function Hex_Image (X : Word_16) return String is
   begin
      return Hex_Image (Word_8 (X / 256)) & Hex_Image (Word_8 (X mod 256));
   end Hex_Image;

   ---------------
   -- Hex_Image --
   ---------------

   function Hex_Image (X : Word_8) return String is
      Ds : constant String := "0123456789ABCDEF";
      It : Word_8 := X;
   begin
      return Image : String (1 .. 2) do
         for Ch of reverse Image loop
            Ch := Ds (Positive (It mod 16 + 1));
            It := It / 16;
         end loop;
      end return;
   end Hex_Image;

   -----------
   -- Image --
   -----------

   function Image (Rec : Pdp11.ISA.Instruction_Record) return String is
      use Pdp11.ISA;
      Instr_Image : constant String := Rec.Instruction'Image;
      Mnemonic    : constant String :=
                      Instr_Image (3 .. Instr_Image'Last);
   begin
      case Rec.Instruction is
         when Double_Operand_Instruction =>
            return Mnemonic
              & (if Rec.Word then "" else "B")
              & " " & Image (Rec.Src)
              & "," & Image (Rec.Dst);
         when Sized_Single_Operand_Instruction =>
            return Mnemonic
              & (if Rec.Word then "" else "B")
              & " " & Image (Rec.Dst);
         when Unsized_Single_Operand_Instruction =>
            return Mnemonic
              & " " & Image (Rec.Dst);
         when Register_Double_Operand_Instruction =>
            return Mnemonic;
         when Branch_Instruction =>
            return Mnemonic;
         when I_SOB =>
            return Mnemonic;
         when I_JMP =>
            return Mnemonic;
         when I_JSR =>
            return Mnemonic;
         when I_RTS =>
            return Mnemonic;
         when I_CCC | I_SCC =>
            return Mnemonic;
         when Floating_Point_F1 =>
            return Mnemonic;
         when Floating_Point_F2 =>
            return Mnemonic;
         when I_INVF =>
            return Mnemonic;
         when Vector_F1 =>
            return Mnemonic
              & " " & VAC_Image (Rec.VAC_1)
              & "," & Image (Rec.V_Operand);
         when Vector_F2 =>
            return Mnemonic
              & " " & VAC_Image (Rec.VAC_1)
              & "," & VAC_Image (Rec.VAC_2);
         when Vector_F3 =>
            return Mnemonic;
      end case;
   end Image;

   -----------
   -- Image --
   -----------

   function Image (Operand : ISA.Operand_Type) return String is
      use Pdp11.ISA;

      Reg_Image : constant String := Register_Image (Operand.Register);

      function Direct_Image return String
      is (case Operand.Mode is
             when Register_Mode => Reg_Image,
             when Autoincrement_Mode => '(' & Reg_Image & ")+",
             when Autodecrement_Mode => "-(" & Reg_Image & ')',
             when Index_Mode => "X(" & Reg_Image & ")");

   begin
      if Operand.Deferred then
         if Operand.Mode = Register_Mode then
            return '(' & Direct_Image & ')';
         else
            return '@' & Direct_Image;
         end if;
      else
         return Direct_Image;
      end if;
   end Image;

   -----------------
   -- Octal_Image --
   -----------------

   function Octal_Image (X : Word_16) return String is
      Ds : constant String := "01234567";
      It : Word_16 := X;
   begin
      return Image : String (1 .. 6) do
         for Ch of reverse Image loop
            Ch := Ds (Positive (It mod 8 + 1));
            It := It / 8;
         end loop;
      end return;
   end Octal_Image;

   --------------------
   -- Register_Image --
   --------------------

   function Register_Image (R : Pdp11.ISA.Register_Index) return String is
      use type Pdp11.ISA.Register_Index;
   begin
      return (if R = 7 then "pc"
              elsif R = 6 then "sp"
              else ('r', Character'Val (48 + Natural (R))));
   end Register_Image;

end Pdp11.Images;
