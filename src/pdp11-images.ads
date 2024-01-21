with Pdp11.ISA;

package Pdp11.Images is

   function Register_Image (R : Pdp11.ISA.Register_Index) return String;
   function FAC_Image (R : Pdp11.ISA.FP_Register_Index) return String
   is ('a', 'c', Character'Val (48 + Natural (R)));

   function VAC_Image (R : Pdp11.ISA.V_Register_Index) return String
   is ('v', Character'Val (48 + Natural (R)));

   function Hex_Image (X : Word_16) return String;
   function Hex_Image (X : Word_8) return String;
   function Octal_Image (X    : Word_32;
      Trim : Boolean)
                         return String;
   function Octal_Image (X : Word_16) return String;
   function Octal_Image (X : Word_8) return String;
   function Image (Rec : Pdp11.ISA.Instruction_Record) return String;

end Pdp11.Images;
