private with Ada.Unchecked_Conversion;

package Pdp11.Conversions is

   function As_Word_32 (X : Float_32) return Word_32;
   function As_Float_32 (X : Word_32) return Float_32;

   function As_Word_16 (X : Integer_16) return Word_16;
   function As_Integer_16 (X : Word_16) return Integer_16;

   function As_Float_32 (Lo, Hi : Word_16) return Float_32;

private

   function To_Word_32 is
     new Ada.Unchecked_Conversion (Float_32, Word_32);

   function As_Word_32 (X : Float_32) return Word_32
   is (To_Word_32 (X));

   function To_Float_32 is
     new Ada.Unchecked_Conversion (Word_32, Float_32);

   function As_Float_32 (X : Word_32) return Float_32
   is (To_Float_32 (X));

   function To_Word_16 is
     new Ada.Unchecked_Conversion (Integer_16, Word_16);

   function As_Word_16 (X : Integer_16) return Word_16
   is (To_Word_16 (X));

   function To_Integer_16 is
     new Ada.Unchecked_Conversion (Word_16, Integer_16);

   function As_Integer_16 (X : Word_16) return Integer_16
   is (To_Integer_16 (X));

   function As_Float_32 (Lo, Hi : Word_16) return Float_32
   is (As_Float_32 (Word_32 (Hi) * 65536 + Word_32 (Lo)));

end Pdp11.Conversions;
