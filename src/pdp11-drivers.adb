package body Pdp11.Drivers is

   --------------------
   -- Install_Driver --
   --------------------

   procedure Install_Driver
     (Driver             : in out Root_Driver_Type'Class;
      Handler            : not null access Interrupt_Handler'Class;
      Interrupt_Priority : Interrupt_Priority_Type;
      Interrupt_Vector   : Address_Type)
   is
   begin
      Driver.Handler := Interrupt_Handler_Reference (Handler);
      Driver.Priority := Interrupt_Priority;
      Driver.Vector := Interrupt_Vector;
   end Install_Driver;

   ---------------
   -- Interrupt --
   ---------------

   procedure Interrupt (Driver : Root_Driver_Type) is
   begin
      if Driver.Handler /= null then
         Driver.Handler.Interrupt
           (Priority => Driver.Priority,
            Vector   => Driver.Vector);
      end if;
   end Interrupt;

end Pdp11.Drivers;
