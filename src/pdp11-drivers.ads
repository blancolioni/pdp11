with Pdp11.Addressable;

package Pdp11.Drivers is

   type Root_Driver_Type is
     abstract new Pdp11.Addressable.Root_Addressable_Type with private;

   type Pdp11_Driver is access all Root_Driver_Type'Class;

   type Interrupt_Handler is interface;

   procedure Interrupt
     (Handler  : in out Interrupt_Handler;
      Priority : Interrupt_Priority_Type;
      Vector   : Address_Type)
   is abstract;

   procedure Install_Driver
     (Driver             : in out Root_Driver_Type'Class;
      Handler            : not null access Interrupt_Handler'Class;
      Interrupt_Priority : Interrupt_Priority_Type;
      Interrupt_Vector   : Address_Type);

   function Name
     (Driver : Root_Driver_Type)
      return String
      is abstract;

   function Bound
     (Driver : Root_Driver_Type)
      return Address_Type
      is abstract;

   procedure Interrupt
     (Driver : Root_Driver_Type);

private

   type Interrupt_Handler_Reference is access all Interrupt_Handler'Class;

   type Root_Driver_Type is
     abstract new Pdp11.Addressable.Root_Addressable_Type with
      record
         Handler  : Interrupt_Handler_Reference;
         Priority : Interrupt_Priority_Type := Interrupt_Priority_Type'First;
         Vector   : Address_Type            := 0;
      end record;

end Pdp11.Drivers;
