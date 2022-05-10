with Pdp11.Drivers;

package Pdp11.Addressable.Memory is

   type Root_Memory_Type is
     new Root_Addressable_Type with private;

   type Memory_Reference is access all Root_Memory_Type'Class;

   function Create
     return Memory_Reference;

   procedure Clear_Drivers
     (This : in out Root_Memory_Type'Class);

   procedure Add_Driver
     (This     : not null access Root_Memory_Type'Class;
      Driver   : not null access Pdp11.Drivers.Root_Driver_Type'Class;
      Base     : Address_Type);

private

   type Driver_Index is range 0 .. 255;

   type Driver_Address_Map is
     array (Address_Type) of Driver_Index
     with Pack, Size => 8 * 65536;

   type Driver_Record is
      record
         Base   : Address_Type            := 0;
         Driver : Pdp11.Drivers.Pdp11_Driver;
      end record;

   type Installed_Driver_Array is
     array (Driver_Index) of Driver_Record;

   type Root_Memory_Type is
     new Root_Addressable_Type with
      record
         Installed_Drivers   : Installed_Driver_Array;
         Driver_Map          : Driver_Address_Map := (others => 0);
      end record;

   overriding function Get_Word_16
     (Memory  : Root_Memory_Type;
      Address : Address_Type)
      return Word_16;

   overriding function Get_Word_8
     (Memory  : Root_Memory_Type;
      Address : Address_Type)
      return Word_8;

   overriding function Get_Float_32
     (Memory  : Root_Memory_Type;
      Address : Address_Type)
      return Float_32;

   overriding procedure Set_Word_16
     (Memory  : in out Root_Memory_Type;
      Address : Address_Type;
      Value   : Word_16);

   overriding procedure Set_Word_8
     (Memory  : in out Root_Memory_Type;
      Address : Address_Type;
      Value   : Word_8);

   overriding procedure Set_Float_32
     (Memory  : in out Root_Memory_Type;
      Address : Address_Type;
      Value   : Float_32);

end Pdp11.Addressable.Memory;
