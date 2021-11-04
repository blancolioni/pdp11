with Pdp11.Conversions;

package body Pdp11.Memory is

   -----------------
   -- Get_Address --
   -----------------

   function Get_Address
     (From    : Root_Memory_Type'Class;
      Address : Address_Type)
      return Address_Type
   is
   begin
      return Address_Type (From.Get_Word_16 (Address));
   end Get_Address;

   ------------------
   -- Get_Float_32 --
   ------------------

   function Get_Float_32
     (From    : Root_Memory_Type;
      Address : Address_Type)
      return Float_32
   is
      From_Class : constant Root_Memory_Type'Class :=
                     Root_Memory_Type'Class (From);
      Lo         : constant Word_16 := From_Class.Get_Word_16 (Address);
      Hi         : constant Word_16 := From_Class.Get_Word_16 (Address + 2);
   begin
      return Conversions.As_Float_32 (Lo, Hi);
   end Get_Float_32;

   ----------------
   -- Get_Word_8 --
   ----------------

   function Get_Word_8
     (From    : Root_Memory_Type;
      Address : Address_Type)
      return Word_8
   is
   begin
      return Word_8
        (Root_Memory_Type'Class (From).Get_Word_16 (Address) mod 256);
   end Get_Word_8;

   -----------------
   -- Get_Word_16 --
   -----------------

   function Get_Word_16
     (From    : Root_Memory_Type;
      Address : Address_Type)
      return Word_16
   is
      M : Root_Memory_Type'Class renames Root_Memory_Type'Class (From);
   begin
      return Word_16 (M.Get_Word_8 (Address))
        + 256 * Word_16 (M.Get_Word_8 (Address + 1));
   end Get_Word_16;

   ------------------
   -- Set_Float_32 --
   ------------------

   procedure Set_Float_32
     (To      : in out Root_Memory_Type;
      Address : Address_Type;
      Value   : Float_32)
   is
      X : constant Word_32 := Conversions.As_Word_32 (Value);
      To_Class : Root_Memory_Type'Class renames
                   Root_Memory_Type'Class (To);
   begin
      To_Class.Set_Word_16 (Address, Word_16 (X mod 65536));
      To_Class.Set_Word_16 (Address + 2, Word_16 (X / 65536));
   end Set_Float_32;

   ----------------
   -- Set_Word_8 --
   ----------------

   procedure Set_Word_8
     (To      : in out Root_Memory_Type;
      Address : Address_Type;
      Value   : Word_8)
   is
      TC : Root_Memory_Type'Class renames Root_Memory_Type'Class (To);
      X : Word_16 := TC.Get_Word_16 (Address);
   begin
      X := (X and not 255) or Word_16 (Value);
      TC.Set_Word_16 (Address, X);
   end Set_Word_8;

   -----------------
   -- Set_Word_16 --
   -----------------

   procedure Set_Word_16
     (To      : in out Root_Memory_Type;
      Address : Address_Type;
      Value   : Word_16)
   is
      M : Root_Memory_Type'Class renames Root_Memory_Type'Class (To);
   begin
      M.Set_Word_8 (Address, Word_8 (Value mod 256));
      M.Set_Word_8 (Address + 1, Word_8 (Value / 256));
   end Set_Word_16;

end Pdp11.Memory;
