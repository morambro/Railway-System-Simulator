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

with Ada.Strings.Unbounded;
with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Vectors;
with Ada.Streams;

package Dashboard.Actions is
   
   --
   --  General exception for reporting problem with remote access.
   --
   Remote_Error : exception;
   
   --
   --  Configuration initialization.
   --
   procedure Init (Remote_Timeout : in Duration);
   
   --
   --  Name server support.
   --
   
   type Name_Server_Entry is record
      Name : Ada.Strings.Unbounded.Unbounded_String;
      Location : Ada.Strings.Unbounded.Unbounded_String;
   end record;
   
   package Name_Server_Entry_Vectors is new Ada.Containers.Vectors
     (Index_Type => Positive,
      Element_Type => Name_Server_Entry);
   
   subtype Name_Server_Entries is Name_Server_Entry_Vectors.Vector;
   
   --
   --  Returns list of name entries from the given name server.
   --
   function Get_Name_Entries (Name_Server_Location : in String)
                             return Name_Server_Entries;
   
   --
   --  Adds new binding to the given name server.
   --
   procedure Add_Name_Server_Entry (Name_Server_Location : in String;
                                    Object : in String;
                                    Object_Location : in String);
   
   --
   --  Broker support.
   --
   
   type Subscriber_Stats is record
      Location : Ada.Strings.Unbounded.Unbounded_String;
      Overflow : Boolean;
      Message_Count : Natural;
      Byte_Count : Ada.Streams.Stream_Element_Count;
   end record;
   
   package Subscriber_Stats_Vectors is new Ada.Containers.Vectors
     (Index_Type => Positive,
      Element_Type => Subscriber_Stats);
      
   type Broker_Stats is record
      Total_Incoming : Ada.Streams.Stream_Element_Count;
      Total_Outgoing : Ada.Streams.Stream_Element_Count;
      Sub_Stats : Subscriber_Stats_Vectors.Vector;
   end record;
   
   --
   --  Returns run-time statistics for all subscriptions.
   --
   function Get_Broker_Stats (Broker_Location : in String)
                             return Broker_Stats;
   
   --
   --  Queue server support.
   --
   
   type Queue_Stats is record
      Name : Ada.Strings.Unbounded.Unbounded_String;
      Num_Of_Messages : Natural;
      Size : Ada.Streams.Stream_Element_Count;
      Num_Of_Clients : Natural;
   end record;
   
   package Queue_Stats_Vectors is new Ada.Containers.Vectors
     (Index_Type => Positive,
      Element_Type => Queue_Stats);
      
   type All_Queue_Stats is record
      Num_Of_Queues : Natural;
      Num_Of_All_Waiting_Messages : Natural;
      Total_Of_All_Content_Sizes : Ada.Streams.Stream_Element_Count;
      Num_Of_All_Waiting_Clients : Natural;
      Queue_Stats : Queue_Stats_Vectors.Vector;
   end record;
   
   --
   --  Returns run-time statistics for all queues.
   --
   function Get_All_Queue_Stats (Queue_Location : in String)
                                 return All_Queue_Stats;
   
   --
   --  Ping and stats support.
   --
   
   package String_Vectors is new Ada.Containers.Indefinite_Vectors
     (Index_Type => Positive,
      Element_Type => String);
   
   type Connection_Stats is record
      Remote : Ada.Strings.Unbounded.Unbounded_String;
      Messages_Sent : Natural;
      Messages_Received : Natural;
      Bytes_Sent : Ada.Streams.Stream_Element_Count;
      Bytes_Received : Ada.Streams.Stream_Element_Count;
   end record;
   
   package Connection_Stats_Vectors is new Ada.Containers.Vectors
     (Index_Type => Positive,
      Element_Type => Connection_Stats);
   
   type Ping_Stats is record
      Connection_Successful : Boolean;
      Connection_Time : Duration;
      
      Message_Successful : Boolean;
      Message_Time : Duration;
      
      Uptime : Duration;
      Listeners : String_Vectors.Vector;
      Objects: String_Vectors.Vector;
      Connections : Connection_Stats_Vectors.Vector;
   end record;
   
   --
   --  Returns statistics for the given remote agent.
   --
   function Ping (Location : in String) return Ping_Stats;
   
end Dashboard.Actions;
