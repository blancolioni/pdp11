with Pdp11.ISA;

package body Pdp11.Devices.Line_Clock is

   Interrupt_Vector   : constant := 8#100#;
   Interrupt_Priority : constant := 6;
   Base_Address       : constant := 8#177546#;
   Bound_Address      : constant := 8#177546#;

   subtype Parent is Pdp11.Devices.Instance;

   type Instance is new Parent with
      record
         Monitor_Bit    : Boolean := True;
         Interrupt_Bit  : Boolean := False;
         Last_Interrupt : Pdp11.ISA.Microsecond_Duration := 0.0;
         Frequency      : Pdp11.ISA.Microsecond_Duration := 50.0;
         Cycle          : Pdp11.ISA.Microsecond_Duration :=
                            1_000_000.0 / 50.0;
         Elapsed        : Pdp11.ISA.Microsecond_Duration := 0.0;
      end record;

   overriding function Name (This : Instance) return String
   is ("line-clock");

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
           Monitor_Bit => <>,
           Interrupt_Bit => <>,
           Last_Interrupt => <>,
           Frequency      => <>,
           Cycle          => <>,
           Elapsed        => <>);
   end Create;

   ----------------
   -- Get_Word_8 --
   ----------------

   overriding procedure Get_Word_8
     (This    : in out Instance;
      Address : Address_Type;
      Value   : out Word_8)
   is
   begin
      if Address = 0 then
         Value := Boolean'Pos (This.Monitor_Bit) * 2 ** 7
           + Boolean'Pos (This.Interrupt_Bit) * 2 ** 6;
         This.Monitor_Bit := False;
      else
         Value := 0;
      end if;
   end Get_Word_8;

   ----------------
   -- Set_Word_8 --
   ----------------

   overriding procedure Set_Word_8
     (This    : in out Instance;
      Address : Address_Type;
      Value   : Word_8)
   is
   begin
      if Address = 0 then
         This.Interrupt_Bit := (Value and 2 ** 6) /= 0;
      end if;
   end Set_Word_8;

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
      if This.Elapsed >= This.Cycle then
         This.Monitor_Bit := True;
         This.Elapsed := This.Elapsed - This.Cycle;
         if This.Interrupt_Bit then
            Handler.Interrupt (Interrupt_Priority, Interrupt_Vector);
         end if;
      end if;
   end Tick;

end Pdp11.Devices.Line_Clock;
