with Pdp11.Devices;

package Pdp11.Addressable.Memory is

   type Root_Memory_Type is
     new Root_Addressable_Type with private;

   type Memory_Reference is access all Root_Memory_Type'Class;

   function Create
     return Memory_Reference;

   procedure Clear_Devices
     (This : in out Root_Memory_Type'Class);

   procedure Add_Device
     (This     : not null access Root_Memory_Type'Class;
      Device   : not null access Pdp11.Devices.Instance'Class);

private

   type Device_Index is range 0 .. 255;

   type Device_Address_Map is
     array (Address_Type) of Device_Index
     with Pack, Size => 8 * 65536;

   type Device_Record is
      record
         Base   : Address_Type            := 0;
         Device : Pdp11.Devices.Reference;
      end record;

   type Installed_Device_Array is
     array (Device_Index) of Device_Record;

   type Root_Memory_Type is
     new Root_Addressable_Type with
      record
         Installed_Devices   : Installed_Device_Array;
         Device_Map          : Device_Address_Map := (others => 0);
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
