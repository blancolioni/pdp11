with Pdp11.ISA;

package body Pdp11.Devices.Line_Clock is

   Interrupt_Vector   : constant := 8#100#;
   Interrupt_Priority : constant := 6;
   Base_Address       : constant := 8#177546#;
   Bound_Address      : constant := 8#177546#;

   subtype Parent is Pdp11.Devices.Instance;

   type Instance is new Parent with
      record
         Last_Interrupt : Pdp11.ISA.Microsecond_Duration := 0.0;
         Frequency      : Pdp11.ISA.Microsecond_Duration := 50.0;
         Elapsed        : Pdp11.ISA.Microsecond_Duration := 0.0;
      end record;

   overriding function Name (This : Instance) return String
   is ("line-clock");

   overriding procedure Tick
     (This    : in out Instance;
      Elapsed : ISA.Microsecond_Duration;
      Handler : not null access Interrupt_Handler'Class);

   ------------
   -- Create --
   ------------

   function Create return Reference is
   begin
      return new Instance'
        (Pdp11.Devices.Parent with
           Priority => Interrupt_Priority,
           Vector   => Interrupt_Vector,
           Base     => Base_Address,
           Bound    => Bound_Address,
           Last_Interrupt => <>,
           Frequency      => <>,
           Elapsed        => <>);
   end Create;

   ----------
   -- Tick --
   ----------

   overriding procedure Tick
     (This    : in out Instance;
      Elapsed : ISA.Microsecond_Duration;
      Handler : not null access Interrupt_Handler'Class)
   is
      use Pdp11.ISA;
   begin
      This.Elapsed := This.Elapsed + Elapsed;
      if This.Elapsed >= This.Frequency then
         Handler.Interrupt (Interrupt_Priority, Interrupt_Vector);
         This.Elapsed := This.Elapsed - This.Frequency;
      end if;
   end Tick;

end Pdp11.Devices.Line_Clock;
