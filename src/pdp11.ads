package Pdp11 is

   type Address_Type is mod 2 ** 16;

   type Word_8 is mod 2 ** 8;
   for Word_8'Size use 8;

   type Word_16 is mod 2 ** 16;
   for Word_16'Size use 16;

   type Word_32 is mod 2 ** 32;
   for Word_32'Size use 32;

   type Float_32 is new Float;
   for Float_32'Size use 32;

   type Integer_16 is range -2 ** 15 .. 2 ** 15 - 1;
   for Integer_16'Size use 16;

   type Priority_Type is mod 8;

   subtype Interrupt_Priority_Type is Priority_Type range 4 .. 7;

end Pdp11;
