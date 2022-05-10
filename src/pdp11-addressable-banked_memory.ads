package Pdp11.Addressable.Banked_Memory is

   type Bank_Index is mod 16;

   type Root_Banked_Memory_Type is
     new Root_Addressable_Type with private;

   type Banked_Memory_Reference is access all Root_Banked_Memory_Type'Class;

   procedure Add_Bank
     (This   : in out Root_Banked_Memory_Type'Class;
      Memory : not null access Root_Addressable_Type'Class);

   procedure Set_Bank
     (This  : in out Root_Banked_Memory_Type'Class;
      Index : Bank_Index);

private

   type Bank_Memory_Array is array (Bank_Index) of Addressable_Reference;

   type Root_Banked_Memory_Type is
     new Root_Addressable_Type with
      record
         Banks        : Bank_Memory_Array;
         Bank_Count   : Natural;
         Current_Bank : Bank_Index := 0;
      end record;

   overriding function Get_Word_16
     (Memory  : Root_Banked_Memory_Type;
      Address : Address_Type)
      return Word_16;

   overriding function Get_Word_8
     (Memory  : Root_Banked_Memory_Type;
      Address : Address_Type)
      return Word_8;

   overriding function Get_Float_32
     (Memory  : Root_Banked_Memory_Type;
      Address : Address_Type)
      return Float_32;

   overriding procedure Set_Word_16
     (Memory  : in out Root_Banked_Memory_Type;
      Address : Address_Type;
      Value   : Word_16);

   overriding procedure Set_Word_8
     (Memory  : in out Root_Banked_Memory_Type;
      Address : Address_Type;
      Value   : Word_8);

   overriding procedure Set_Float_32
     (Memory  : in out Root_Banked_Memory_Type;
      Address : Address_Type;
      Value   : Float_32);

end Pdp11.Addressable.Banked_Memory;
