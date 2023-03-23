package Pdp11.Addressable is

   Bad_Address : exception;

   type Root_Addressable_Type is abstract tagged private;
   type Addressable_Reference is access all Root_Addressable_Type'Class;

   procedure Get_Word_8
     (From    : in out Root_Addressable_Type;
      Address : Address_Type;
      Value   : out Word_8);

   procedure Get_Word_16
     (From    : in out Root_Addressable_Type;
      Address : Address_Type;
      Value   : out Word_16);

   procedure Get_Float_32
     (From    : in out Root_Addressable_Type;
      Address : Address_Type;
      Value   : out Float_32);

   procedure Get_Address
     (From    : in out Root_Addressable_Type;
      Address : Address_Type;
      Value   : out Address_Type);

   procedure Set_Word_8
     (To      : in out Root_Addressable_Type;
      Address : Address_Type;
      Value   : Word_8);

   procedure Set_Word_16
     (To      : in out Root_Addressable_Type;
      Address : Address_Type;
      Value   : Word_16);

   procedure Set_Float_32
     (To      : in out Root_Addressable_Type;
      Address : Address_Type;
      Value   : Float_32);

private

   type Root_Addressable_Type is abstract tagged null record;

end Pdp11.Addressable;
