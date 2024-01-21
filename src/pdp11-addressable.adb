with Pdp11.Conversions;

package body Pdp11.Addressable is

   -----------------
   -- Get_Address --
   -----------------

   procedure Get_Address
     (From    : in out Root_Addressable_Type;
      Address : Address_Type;
      Value   : out Address_Type)
   is
      Word : Word_16;
   begin
      Root_Addressable_Type'Class (From).Get_Word_16 (Address, Word);
      Value := Address_Type (Word);
   end Get_Address;

   ------------------
   -- Get_Float_32 --
   ------------------

   procedure Get_Float_32
     (From    : in out Root_Addressable_Type;
      Address : Address_Type;
      Value   : out Float_32)
   is
      From_Class : Root_Addressable_Type'Class renames
                     Root_Addressable_Type'Class (From);
      Lo, Hi     : Word_16;
   begin
      From_Class.Get_Word_16 (Address, Lo);
      From_Class.Get_Word_16 (Address + 2, Hi);
      Value := Conversions.As_Float_32 (Lo, Hi);
   end Get_Float_32;

   ----------------
   -- Get_Word_8 --
   ----------------

   procedure Get_Word_8
     (From    : in out Root_Addressable_Type;
      Address : Address_Type;
      Value   : out Word_8)
   is
      Word : Word_16;
   begin
      Root_Addressable_Type'Class (From).Get_Word_16
        (Address - Address mod 2, Word);
      if Address mod 2 = 0 then
         Value := Word_8 (Word mod 256);
      else
         Value := Word_8 (Word / 256);
      end if;
   end Get_Word_8;

   -----------------
   -- Get_Word_16 --
   -----------------

   procedure Get_Word_16
     (From    : in out Root_Addressable_Type;
      Address : Address_Type;
      Value   : out Word_16)
   is
      M : Root_Addressable_Type'Class renames
            Root_Addressable_Type'Class (From);
      Lo, Hi : Word_8;
   begin
      M.Get_Word_8 (Address, Lo);
      M.Get_Word_8 (Address + 1, Hi);
      Value := Word_16 (Lo) + Word_16 (Hi) * 256;
   end Get_Word_16;

   ------------------
   -- Set_Float_32 --
   ------------------

   procedure Set_Float_32
     (To      : in out Root_Addressable_Type;
      Address : Address_Type;
      Value   : Float_32)
   is
      X : constant Word_32 := Conversions.As_Word_32 (Value);
      To_Class : Root_Addressable_Type'Class renames
                   Root_Addressable_Type'Class (To);
   begin
      To_Class.Set_Word_16 (Address, Word_16 (X mod 65536));
      To_Class.Set_Word_16 (Address + 2, Word_16 (X / 65536));
   end Set_Float_32;

   ----------------
   -- Set_Word_8 --
   ----------------

   procedure Set_Word_8
     (To      : in out Root_Addressable_Type;
      Address : Address_Type;
      Value   : Word_8)
   is
      TC : Root_Addressable_Type'Class renames
             Root_Addressable_Type'Class (To);
      X : Word_16;
   begin
      TC.Get_Word_16 (Address - Address mod 2, X);
      if Address mod 2 = 0 then
         X := (X and not 255) or Word_16 (Value);
      else
         X := X mod 256 + Word_16 (Value) * 256;
      end if;
      TC.Set_Word_16 (Address - Address mod 2, X);
   end Set_Word_8;

   -----------------
   -- Set_Word_16 --
   -----------------

   procedure Set_Word_16
     (To      : in out Root_Addressable_Type;
      Address : Address_Type;
      Value   : Word_16)
   is
      M : Root_Addressable_Type'Class renames Root_Addressable_Type'Class (To);
   begin
      M.Set_Word_8 (Address, Word_8 (Value mod 256));
      M.Set_Word_8 (Address + 1, Word_8 (Value / 256));
   end Set_Word_16;

end Pdp11.Addressable;
