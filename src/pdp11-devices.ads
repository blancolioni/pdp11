with Pdp11.Addressable;
with Pdp11.ISA;

package Pdp11.Devices is

   subtype Parent is Pdp11.Addressable.Root_Addressable_Type;
   type Instance is abstract new Parent with private;

   type Reference is access all Instance'Class;

   type Interrupt_Handler is interface;

   procedure Interrupt
     (Handler  : in out Interrupt_Handler;
      Priority : Interrupt_Priority_Type;
      Vector   : Address_Type)
   is abstract;

   function Name
     (This : Instance)
      return String
      is abstract;

   procedure Tick
     (This    : in out Instance;
      Elapsed : ISA.Microsecond_Duration;
      Handler : not null access Interrupt_Handler'Class)
   is abstract;

   function Base
     (This : Instance)
      return Address_Type;

   function Bound
     (This : Instance)
      return Address_Type;

   procedure Interrupt
     (This    : Instance'Class;
      Handler : not null access Interrupt_Handler'Class);

   type Device_Manager is interface;

   procedure Clear_Devices
     (This : in out Device_Manager)
   is abstract;

   procedure Add_Device
     (This     : in out Device_Manager;
      Device   : not null access Pdp11.Devices.Instance'Class)
   is abstract;

private

   type Instance is
     abstract new Pdp11.Addressable.Root_Addressable_Type with
      record
         Priority : Interrupt_Priority_Type := Interrupt_Priority_Type'First;
         Vector   : Address_Type            := 0;
         Base     : Address_Type;
         Bound    : Address_Type;
      end record;

   function Base
     (This : Instance)
      return Address_Type
   is (This.Base);

   function Bound
     (This : Instance)
      return Address_Type
   is (This.Bound);

end Pdp11.Devices;
