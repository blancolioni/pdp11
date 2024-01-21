package body Pdp11.Devices.Plugins is

   ----------------
   -- Get_Word_8 --
   ----------------

   overriding procedure Get_Word_8
     (This    : in out Instance;
      Address : Address_Type;
      Value   : out Word_8)
   is
   begin
      Value := Word_8 (This.Registers (Storage_Count (Address)));
   end Get_Word_8;

   ----------
   -- Load --
   ----------

   function Load
     (Bus     : not null access
        Pdp11.Addressable.Root_Addressable_Type'Class;
      Base    : Address_Type;
      Ref     : Device_Reference)
      return Reference
   is
      pragma Unreferenced (Bus);
   begin
      return new Instance'
        (Pdp11.Devices.Parent with
           Priority  => <>,
         Vector    => <>,
         Base      => Base,
         Bound     => Base + Address_Type (Storage_Bound),
         Ref       => Ref,
         Registers => (others => 0));
   end Load;

   ----------------
   -- Set_Word_8 --
   ----------------

   overriding procedure Set_Word_8
     (This    : in out Instance;
      Address : Address_Type;
      Value   : Word_8)
   is
   begin
      This.Registers (Storage_Count (Address)) := Storage_Element (Value);
   end Set_Word_8;

   ----------
   -- Tick --
   ----------

   overriding procedure Tick
     (This    : in out Instance;
      Elapsed : ISA.Microsecond_Duration;
      Handler : not null access Interrupt_Handler'Class)
   is
      Item : Device_Registers;
      pragma Import (Ada, Item);
      for Item'Address use This.Registers'Address;
   begin
      Update (Item, This.Ref, Duration (Elapsed) / 1.0e6);
   end Tick;

end Pdp11.Devices.Plugins;
