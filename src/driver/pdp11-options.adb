with WL.Command_Line;

package body Pdp11.Options is

   pragma Style_Checks (Off);

   function Base_Address return Natural is
   begin
      return WL.Command_Line.Find_Option
               ("base-address", ' ', 0);
   end Base_Address;

   function Execute return Boolean is
   begin
      return WL.Command_Line.Find_Option
               ("execute", ' ');
   end Execute;

   function Object return String is
   begin
      return WL.Command_Line.Find_Option
               ("object", ' ');
   end Object;

   function Quiet return Boolean is
   begin
      return WL.Command_Line.Find_Option
               ("quiet", ' ');
   end Quiet;

   function Source return String is
   begin
      return WL.Command_Line.Find_Option
               ("source", ' ');
   end Source;

   function Test_Encoding return Boolean is
   begin
      return WL.Command_Line.Find_Option
               ("test-encoding", ' ');
   end Test_Encoding;

   function Time_Limit return Natural is
   begin
      return WL.Command_Line.Find_Option
               ("time-limit", ' ', 0);
   end Time_Limit;

   function Trace return Boolean is
   begin
      return WL.Command_Line.Find_Option
               ("trace", ' ');
   end Trace;

   function Warnings return Boolean is
   begin
      return WL.Command_Line.Find_Option
               ("warnings", ' ');
   end Warnings;

end Pdp11.Options;
