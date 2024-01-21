private with System.Storage_Elements;

with Pdp11.Addressable;

generic
   Plugin_Name : String;
   type Device_Reference is private;
   type Device_Registers is private;
   with procedure Update
     (This      : in out Device_Registers;
      Reference : Device_Reference;
      Elapsed   : Duration);
package Pdp11.Devices.Plugins is

   subtype Parent is Pdp11.Devices.Instance;
   type Instance is new Parent with private;

   type Reference is access all Instance'Class;

   function Load
     (Bus     : not null access
        Pdp11.Addressable.Root_Addressable_Type'Class;
      Base    : Address_Type;
      Ref     : Device_Reference)
      return Reference;

private

   use System.Storage_Elements;

   Register_Count : constant Storage_Count :=
                      Device_Registers'Size / System.Storage_Unit;

   Storage_Bound  : constant Storage_Count := Register_Count - 1;

   subtype Register_Storage is
     Storage_Array (0 .. Storage_Bound);

   type Instance is new Parent with
      record
         Ref       : Device_Reference;
         Registers : Register_Storage;
      end record;

   overriding function Name
     (This : Instance)
      return String
   is (Plugin_Name);

   overriding procedure Tick
     (This    : in out Instance;
      Elapsed : ISA.Microsecond_Duration;
      Handler : not null access Interrupt_Handler'Class);

   overriding procedure Get_Word_8
     (This    : in out Instance;
      Address : Address_Type;
      Value   : out Word_8);

   overriding procedure Set_Word_8
     (This    : in out Instance;
      Address : Address_Type;
      Value   : Word_8);

end Pdp11.Devices.Plugins;
