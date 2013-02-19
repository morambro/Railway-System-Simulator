--  Copyright Maciej Sobczak 2008-2012.
--  This file is part of YAMI4.
--
--  YAMI4 is free software: you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation, either version 3 of the License, or
--  (at your option) any later version.
--
--  YAMI4 is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with YAMI4.  If not, see <http://www.gnu.org/licenses/>.

with Log;

with YAMI.Agents;
with YAMI.Option_Names;
with YAMI.Outgoing_Messages;
with YAMI.Parameters;

with Ada.Calendar;
with Ada.Exceptions;

package body Dashboard.Actions is
   
   --  config options for lightweight agent (strict client-server)
   
   Lightweight_Options : YAMI.Parameters.Parameters_Collection :=
     YAMI.Parameters.Make_Parameters;
   
   --  configurable timeout value for remote calls
   
   Remote_Calls_Timeout : Duration;
   
   procedure Init (Remote_Timeout : in Duration) is
   begin
      Remote_Calls_Timeout := Remote_Timeout;
   end Init;
   
   function Get_Name_Entries (Name_Server_Location : in String)
                             return Name_Server_Entries is
      
      Agent : YAMI.Agents.Agent :=
        YAMI.Agents.Make_Agent (Lightweight_Options);
      
      Msg : aliased YAMI.Outgoing_Messages.Outgoing_Message;
      
      Result : Name_Server_Entries;
      
      procedure Process_Reply
        (Content : in out YAMI.Parameters.Parameters_Collection) is
         
         Length : constant YAMI.Parameters.Count_Type :=
           Content.Get_String_Array_Length ("object");
      begin
         for I in YAMI.Parameters.Index_Type range 1 .. Length loop
            declare
               Name : constant String :=
                 Content.Get_String_In_Array ("object", I);
               Location : constant String :=
                 Content.Get_String_In_Array ("location", I);
            begin
               Result.Append
                 (Name_Server_Entry'
                    (Ada.Strings.Unbounded.To_Unbounded_String (Name),
                     Ada.Strings.Unbounded.To_Unbounded_String (Location)));
            end;
         end loop;
      end Process_Reply;
      
      use type YAMI.Outgoing_Messages.Message_State;
            
   begin
      Log.Put (Remote,
               "Getting list of names from name server at " &
                 Name_Server_Location);
      
      begin
         Agent.Send
           (Name_Server_Location, "names", "list",
            Msg'Unchecked_Access);
         
         select
            Msg.Wait_For_Completion;
         or
            delay Remote_Calls_Timeout;
            raise Remote_Error with "Operation timed out.";
         end select;
      
         if Msg.State = YAMI.Outgoing_Messages.Replied then
            Msg.Process_Reply_Content (Process_Reply'Access);
         end if;
      exception
         when E : others =>
            declare
               Msg : constant String := Ada.Exceptions.Exception_Message (E);
            begin
               Log.Put (Remote, "Error during remote operation: " & Msg);
            
               raise Remote_Error with Msg;
            end;
      end;
      
      return Result;
   end Get_Name_Entries;
   
   procedure Add_Name_Server_Entry (Name_Server_Location : in String;
                                    Object : in String;
                                    Object_Location : in String) is
      
      Agent : YAMI.Agents.Agent :=
        YAMI.Agents.Make_Agent (Lightweight_Options);
      
      Params : YAMI.Parameters.Parameters_Collection :=
        YAMI.Parameters.Make_Parameters;
      
      Msg : aliased YAMI.Outgoing_Messages.Outgoing_Message;
      
      use type YAMI.Outgoing_Messages.Message_State;
            
   begin
      Log.Put (Remote,
               "Adding new entry (" & Object & " -> " & Object_Location &
                 ") to " & Name_Server_Location);
      
      Params.Set_String ("object", Object);
      Params.Set_String ("location", Object_Location);
      
      Agent.Send
        (Name_Server_Location, "names", "bind", Params,
         Msg'Unchecked_Access);
      
      select
         Msg.Wait_For_Completion;
      or
         delay Remote_Calls_Timeout;
         raise Remote_Error with "Operation timed out.";
      end select;
      
      
      if Msg.State = YAMI.Outgoing_Messages.Rejected then
         raise Remote_Error with Msg.Exception_Message;
      end if;
   exception
      when E : others =>
         declare
            Msg : constant String := Ada.Exceptions.Exception_Message (E);
         begin
            Log.Put (Remote, "Error during remote operation: " & Msg);
            
            raise Remote_Error with Msg;
         end;
   end Add_Name_Server_Entry;
   
   function Get_Broker_Stats (Broker_Location : in String)
                             return Broker_Stats is
      
      Agent : YAMI.Agents.Agent :=
        YAMI.Agents.Make_Agent (Lightweight_Options);

      Msg : aliased YAMI.Outgoing_Messages.Outgoing_Message;
      
      Result : Broker_Stats;

      procedure Process_Reply
        (Content : in out YAMI.Parameters.Parameters_Collection) is

         Overflows_Field_Name : constant String := "overflows";

         Dummy_Entry : YAMI.Parameters.Parameter_Entry;
         Subscription_Details_Found : Boolean;

         procedure Process_Subscription_Details is

            Active_Subscription_Count : constant YAMI.Parameters.Count_Type :=
              Content.Get_Boolean_Array_Length (Overflows_Field_Name);

            type Overflow_Array_Type is
              array (YAMI.Parameters.Index_Type range <>) of Boolean;

            type Sent_Counters_Array_Type is
              array (YAMI.Parameters.Index_Type range <>) of
              YAMI.Parameters.YAMI_Long_Long_Integer;

            procedure Get_Boolean_Array is
               new YAMI.Parameters.Get_Boolean_Array
              (Index_Type => YAMI.Parameters.Index_Type,
               Boolean_Array_Type => Overflow_Array_Type);

            procedure Get_Long_Long_Array is
               new YAMI.Parameters.Get_Long_Long_Array
              (Index_Type => YAMI.Parameters.Index_Type,
               Long_Long_Integer_Type =>
                 YAMI.Parameters.YAMI_Long_Long_Integer,
               Long_Long_Integer_Array_Type => Sent_Counters_Array_Type);

            Overflow_Array : Overflow_Array_Type
              (1 .. Active_Subscription_Count);
            Sent_Messages_Array : Sent_Counters_Array_Type
              (1 .. Active_Subscription_Count);
            Sent_Bytes_Array : Sent_Counters_Array_Type
              (1 .. Active_Subscription_Count);

         begin
            Get_Boolean_Array
              (Content, Overflows_Field_Name, Overflow_Array);
            Get_Long_Long_Array
              (Content, "sent_messages", Sent_Messages_Array);
            Get_Long_Long_Array
              (Content, "sent_bytes", Sent_Bytes_Array);

            for I in 1 .. Active_Subscription_Count loop
               declare
                  Location : constant String :=
                    Content.Get_String_In_Array ("locations", I);
                  
                  Overflow : constant Boolean := Overflow_Array (I);
                  Message_Count : constant Natural :=
                    Natural (Sent_Messages_Array (I));
                  Bytes_Count : constant Ada.Streams.Stream_Element_Count :=
                    Ada.Streams.Stream_Element_Count (Sent_Bytes_Array (I));
               begin
                  Result.Sub_Stats.Append
                    (Subscriber_Stats'
                       (Ada.Strings.Unbounded.To_Unbounded_String (Location),
                        Overflow,
                        Message_Count,
                        Bytes_Count));
               end;
            end loop;
         end Process_Subscription_Details;
         
      begin
         Result.Total_Incoming :=
           Ada.Streams.Stream_Element_Count
           (Content.Get_Long_Long ("total_incoming"));
         Result.Total_Outgoing :=
           Ada.Streams.Stream_Element_Count
           (Content.Get_Long_Long ("total_outgoing"));

         Content.Find
           (Overflows_Field_Name, Dummy_Entry, Subscription_Details_Found);

         if Subscription_Details_Found then
            Process_Subscription_Details;
         end if;

      end Process_Reply;

      use type YAMI.Outgoing_Messages.Message_State;

   begin
      Log.Put (Remote,
               "Getting statistics for broker at " &
                 Broker_Location);
      
      begin
         Agent.Send
           (Broker_Location, "stats", "get-details", Msg'Unchecked_Access);

         select
            Msg.Wait_For_Completion;
         or
            delay Remote_Calls_Timeout;
            raise Remote_Error with "Operation timed out.";
         end select;
         
         if Msg.State = YAMI.Outgoing_Messages.Replied then
            Msg.Process_Reply_Content (Process_Reply'Access);
         end if;
      exception
         when E : others =>
            declare
               Msg : constant String := Ada.Exceptions.Exception_Message (E);
            begin
               Log.Put (Remote, "Error during remote operation: " & Msg);
            
               raise Remote_Error with Msg;
            end;
      end;
      
      return Result;
   end Get_Broker_Stats;
   
   function Get_All_Queue_Stats (Queue_Location : in String)
                                 return All_Queue_Stats is
      
      Agent : YAMI.Agents.Agent :=
        YAMI.Agents.Make_Agent (Lightweight_Options);

      Msg : aliased YAMI.Outgoing_Messages.Outgoing_Message;
      
      Result : All_Queue_Stats;

      procedure Process_Reply
        (Content : in out YAMI.Parameters.Parameters_Collection) is

         procedure Process_Details
           (Num_Of_Queues : in YAMI.Parameters.Count_Type) is

            type Counter_Array_Type is
               array (YAMI.Parameters.Index_Type range <>)
               of Natural;
      
            type Size_Array_Type is
               array (YAMI.Parameters.Index_Type range <>)
               of Ada.Streams.Stream_Element_Count;
      
            Num_Of_Waiting_Messages_Array :
               Counter_Array_Type
                 (1 .. YAMI.Parameters.Index_Type (Num_Of_Queues));
            Total_Content_Sizes_Array :
               Size_Array_Type
                 (1 .. YAMI.Parameters.Index_Type (Num_Of_Queues));
            Num_Of_Waiting_Clients_Array :
               Counter_Array_Type
                 (1 .. YAMI.Parameters.Index_Type (Num_Of_Queues));
      
            procedure Get_Counters_Array is
               new YAMI.Parameters.Get_Integer_Array
                 (Index_Type => YAMI.Parameters.Index_Type,
                  Integer_Type => Natural,
                  Integer_Array_Type => Counter_Array_Type);
      
            procedure Get_Sizes_Array is
               new YAMI.Parameters.Get_Long_Long_Array
                 (Index_Type => YAMI.Parameters.Index_Type,
                  Long_Long_Integer_Type => Ada.Streams.Stream_Element_Count,
                  Long_Long_Integer_Array_Type => Size_Array_Type);

         begin
            Get_Counters_Array
              (Content, "num_of_waiting_messages", Num_Of_Waiting_Messages_Array);
            Get_Sizes_Array
              (Content, "total_content_sizes", Total_Content_Sizes_Array);
            Get_Counters_Array
              (Content, "num_of_waiting_clients", Num_Of_Waiting_Clients_Array);

            for I in 1 .. Num_Of_Queues loop
               declare
                  Name : constant String :=
                    Content.Get_String_In_Array ("queue_names", I);

                  Num_Of_Waiting_Messages : constant Natural :=
                    Num_Of_Waiting_Messages_Array (I);
                  
                  Total_Content_Size : constant Ada.Streams.Stream_Element_Count :=
                    Ada.Streams.Stream_Element_Count (Total_Content_Sizes_Array (I));

                  Num_Of_Waiting_Clients : constant Natural :=
                    Num_Of_Waiting_Clients_Array (I);
               begin
                  Result.Queue_Stats.Append
                    (Queue_Stats'
                       (Ada.Strings.Unbounded.To_Unbounded_String (Name),
                        Num_Of_Waiting_Messages,
                        Total_Content_Size,
                        Num_Of_Waiting_Clients));
               end;
            end loop;
         end Process_Details;
         
      begin
         Result.Num_Of_Queues :=
           Natural (Content.Get_Integer ("num_of_queues"));
         Result.Num_Of_All_Waiting_Messages :=
           Natural (Content.Get_Integer ("num_of_all_waiting_messages"));
         Result.Total_Of_All_Content_Sizes :=
           Ada.Streams.Stream_Element_Count
           (Content.Get_Long_Long ("total_of_all_content_sizes"));
         Result.Num_Of_All_Waiting_Clients :=
           Natural (Content.Get_Integer ("num_of_all_waiting_clients"));

         if Result.Num_Of_Queues /= 0 then
            Process_Details (YAMI.Parameters.Count_Type (Result.Num_Of_Queues));
         end if;

      end Process_Reply;

      use type YAMI.Outgoing_Messages.Message_State;

   begin
      Log.Put (Remote,
               "Getting statistics for queue server at " &
                 Queue_Location);
      
      begin
         Agent.Send
           (Queue_Location, "stats", "get-details", Msg'Unchecked_Access);

         select
            Msg.Wait_For_Completion;
         or
            delay Remote_Calls_Timeout;
            raise Remote_Error with "Operation timed out.";
         end select;
         
         if Msg.State = YAMI.Outgoing_Messages.Replied then
            Msg.Process_Reply_Content (Process_Reply'Access);
         end if;
      exception
         when E : others =>
            declare
               Msg : constant String := Ada.Exceptions.Exception_Message (E);
            begin
               Log.Put (Remote, "Error during remote operation: " & Msg);
            
               raise Remote_Error with Msg;
            end;
      end;
      
      return Result;
   end Get_All_Queue_Stats;
   
   function Ping (Location : in String) return Ping_Stats is
      
      Agent : YAMI.Agents.Agent :=
        YAMI.Agents.Make_Agent (Lightweight_Options);

      Msg : aliased YAMI.Outgoing_Messages.Outgoing_Message;
      
      Start : Ada.Calendar.Time;
      Stop : Ada.Calendar.Time;
      
      Result : Ping_Stats;

      procedure Process_Reply
        (Content : in out YAMI.Parameters.Parameters_Collection) is
         
         Length : YAMI.Parameters.Count_Type;
         
         Listeners_Field : constant String := "listeners";
         Objects_Field : constant String := "objects";
         Conn_Names_Field : constant String := "connection_names";
         
         type Counters_Array_Type is
           array (YAMI.Parameters.Index_Type range <>) of
           YAMI.Parameters.YAMI_Long_Long_Integer;
         
         procedure Get_Long_Long_Array is
            new YAMI.Parameters.Get_Long_Long_Array
           (Index_Type => YAMI.Parameters.Index_Type,
            Long_Long_Integer_Type =>
              YAMI.Parameters.YAMI_Long_Long_Integer,
            Long_Long_Integer_Array_Type => Counters_Array_Type);
         
      begin
         Result.Uptime := Duration (Content.Get_Integer ("uptime"));
         
         --  listeners
         
         Length := Content.Get_String_Array_Length (Listeners_Field);
         for I in 1 .. Length loop
            Result.Listeners.Append
              (Content.Get_String_In_Array (Listeners_Field, I));
         end loop;
         
         --  objects
         
         Length := Content.Get_String_Array_Length (Objects_Field);
         for I in 1 .. Length loop
            Result.Objects.Append
              (Content.Get_String_In_Array (Objects_Field, I));
         end loop;
         
         --  connections
         
         Length := Content.Get_String_Array_Length (Conn_Names_Field);
         
         declare
            Messages_Sent_Array : Counters_Array_Type (1 .. Length);
            Messages_Received_Array : Counters_Array_Type (1 .. Length);
            Bytes_Sent_Array : Counters_Array_Type (1 .. Length);
            Bytes_Received_Array : Counters_Array_Type (1 .. Length);
         begin
            Get_Long_Long_Array
              (Content, "messages_sent", Messages_Sent_Array);
            Get_Long_Long_Array
              (Content, "messages_received", Messages_Received_Array);
            Get_Long_Long_Array
              (Content, "bytes_sent", Bytes_Sent_Array);
            Get_Long_Long_Array
              (Content, "bytes_received", Bytes_Received_Array);
            
            for I in 1 .. Length loop
               Result.Connections.Append
                 (Connection_Stats'
                    (Remote => Ada.Strings.Unbounded.To_Unbounded_String
                       (Content.Get_String_In_Array (Conn_Names_Field, I)),
                     Messages_Sent => Integer (Messages_Sent_Array (I)),
                     Messages_Received => Integer (Messages_Received_Array (I)),
                     Bytes_Sent => Ada.Streams.Stream_Element_Count
                       (Bytes_Sent_Array (I)),
                     Bytes_Received => Ada.Streams.Stream_Element_Count
                       (Bytes_Received_Array (I))));
            end loop;
         end;
      exception
         when others =>
            --  improper or inconsistent stats values
            --  - ignore the reply as a whole
            Result.Message_Successful := False;
      end Process_Reply;
      
      use type Ada.Calendar.Time;
      use type YAMI.Outgoing_Messages.Message_State;

   begin
      Log.Put (Remote, "Getting ping/statistics for agent at " & Location);
      
      begin
         Start := Ada.Calendar.Clock;
         Agent.Open_Connection (Location);
         Stop := Ada.Calendar.Clock;
         
         Result.Connection_Successful := True;
         Result.Connection_Time := Stop - Start;
      exception
         when others =>
            Result.Connection_Successful := False;
      end;
      
      begin
         Start := Ada.Calendar.Clock;
         Agent.Send (Location, "stats", "get", Msg'Unchecked_Access);
         
         select
            Msg.Wait_For_Completion;
         or
            delay Remote_Calls_Timeout;
            raise Remote_Error with "Operation timed out.";
         end select;
               
         Stop := Ada.Calendar.Clock;

         if Msg.State /= YAMI.Outgoing_Messages.Replied then
            Result.Message_Successful := False;
         else
            Result.Message_Successful := True;
            Result.Message_Time := Stop - Start;
            
            Msg.Process_Reply_Content (Process_Reply'Access);
         end if;
         
      exception
         when E : others =>
            declare
               Msg : constant String := Ada.Exceptions.Exception_Message (E);
            begin
               Log.Put (Remote, "Error during remote operation: " & Msg);
               Result.Message_Successful := False;
            end;
      end;
      
      return Result;
   end Ping;
   
begin
   Lightweight_Options.Set_Integer (YAMI.Option_Names.Dispatcher_Threads, 0);
end Dashboard.Actions;
