package body Pdp11.Devices is

   ---------------
   -- Interrupt --
   ---------------

   procedure Interrupt
     (This    : Instance'Class;
      Handler : not null access Interrupt_Handler'Class)
   is
   begin
      Handler.Interrupt
        (Priority => This.Priority,
         Vector   => This.Vector);
   end Interrupt;

end Pdp11.Devices;
