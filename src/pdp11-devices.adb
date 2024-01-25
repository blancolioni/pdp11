package body Pdp11.Devices is

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (This     : in out Instance'Class;
      Base     : Address_Type;
      Bound    : Address_Type;
      Priority : Interrupt_Priority_Type := Interrupt_Priority_Type'First;
      Vector   : Address_Type            := 0)
   is
   begin
      This.Base := Base;
      This.Bound := Bound;
      This.Priority := Priority;
      This.Vector := Vector;
   end Initialize;

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
