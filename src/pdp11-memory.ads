package Pdp11.Memory is

   type Root_Memory_Type is abstract tagged private;

   function Get_Word_8
     (From    : Root_Memory_Type;
      Address : Address_Type)
      return Word_8;

   function Get_Word_16
     (From    : Root_Memory_Type;
      Address : Address_Type)
      return Word_16;

   function Get_Float_32
     (From    : Root_Memory_Type;
      Address : Address_Type)
      return Float_32;

   function Get_Address
     (From    : Root_Memory_Type'Class;
      Address : Address_Type)
      return Address_Type;

   procedure Set_Word_8
     (To      : in out Root_Memory_Type;
      Address : Address_Type;
      Value   : Word_8);

   procedure Set_Word_16
     (To      : in out Root_Memory_Type;
      Address : Address_Type;
      Value   : Word_16);

   procedure Set_Float_32
     (To      : in out Root_Memory_Type;
      Address : Address_Type;
      Value   : Float_32);

private

   type Root_Memory_Type is abstract tagged null record;

end Pdp11.Memory;
