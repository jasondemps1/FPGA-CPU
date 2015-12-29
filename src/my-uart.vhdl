library ieee;
use ieee.numeric_std.all;
use ieee.logic_1164.all;

use helpers.all;

-- UART module, does UART things. (8-bit UART currently)
-- Feed me words and I'll send'em all!.
-- Likewise, tell me how much to receive + fifo.

-- Problem: Clock might run faster than the one driving the computer.
--          Computer might not receive fast enough.

entity UART is
  generic(
    FIFO_SZ : natural;
    WORD_SZ : natural
    );
  port(
    clock     : in     std_logic;
    bit_recv  : in     std_logic;
    byte_recv : buffer std_logic_vector(7 downto 0);
    bit_send  : out    std_logic;
    word_send : out    std_logic_vector(WORD_SZ-1 downto 0);
    transmit  : in     std_logic;
    recv      : out    std_logic;
-- FIFOs might just be internal to UART module.......
   --fifo_recv : in     fifo(FIFO_SZ);
   --fifo_send : out    fifo(FIFO_SZ)
    );
end UART;

architecture UART of UART is
  type state_t is (recv, send);         -- Default is recv.
  type process_state_t is (idle, start, payload, fin);

  signal curr_state, next_state           : state_t         := recv;
  signal curr_send_state, next_send_state : process_state_t := idle;
  signal curr_recv_state, next_recv_state : process_state_t := idle;

  signal byte_send_sr, byte_recv_sr : std_logic_vector(byte_recv'left downto 0);

  constant NUM_BYTES    : natural := WORD_SZ / byte_recv'length;
  constant UNALIGN_BITS : natural := WORD_SZ % byte_recv'length;
begin

  -- TODO: Determine if we should recv / send
  -- TODO: Maybe send should be default state?
  process (bit_recv, curr_recv_state)
  begin
    -- Try to go to receive state.
    -- Just check if recv goes low and we're in default state. (Implies start condition)
    if bit_recv = '0' and curr_state <= send then
      curr_state <= recv;
    end if;

    curr_state <= send;

  end process;


  -- Send only at the moment.
  process (clock)
    variable word_amt             : natural;
    variable shift_amt, shift_max : natural;
    variable byte_inc             : std_logic_vector(7 downto 0);
    variable send_unalign         : std_logic;
  begin
    case (curr_state) is
      when recv =>
        case (curr_recv_state) is
          when idle =>
            shift_amt    := '0';
            byte_recv_sr <= (others => '0');

            if bit_recv = '0' then
              next_recv_state <= payload;
            end if;
          when start =>                 -- Might not need this state.

          when payload =>
            if shift_amt < byte_recv'length then
              byte_recv_sr(shift_amt) <= bit_recv;
              byte_recv_sr            <= byte_recv_sr sll 1;
            else
              next_recv_state <= fin;
            end if;

          when fin =>
            byte_recv <= byte_recv_sr;
            -- TODO: Flag that new data is available.

        end case;
      when send =>
        -- TODO: Handle the unaligned stuff! (Might be done?)
        case (curr_send_state) is
          when idle =>
            -- Keep the line high
            bit_send <= '1';

            byte_send_sr <= (others => '0');
            word_amt     <= 0;

            -- TODO: How do we get to start?

          when start =>
            -- Start Condition (Go low)
            bit_send <= '0';

            next_send_state <= payload;

            send_unalign := '0';

            -- Grab next byte, based on how many words we've done.
            if word_amt < NUM_BYTES then
              byte_send_sr <= word_send(byte_recv'length * word_amt - 1 downto byte_recv'length * word_amt - byte_recv'length);  -- This won't work......I guarantee it.
              shift_max    <= byte_recv'length;
            else
              -- Grab unaligned bits
              if UNALIGN_BITS > 0 then
                send_unalign := '1';
                byte_send_sr <= word_send(word_send'left downto word_send'left - UNALIGN_BITS);
              else
                -- We're actually done......I can't believe it!
                bit_send        <= '1';
                next_send_state <= idle;
              end if;
            end if;

            shift_amt := 0;

          when payload =>
            if shift_amt < shift_max then
              bit_send  <= byte_send_sr(shift_amt);
              shift_amt := shift_amt + 1;
            else
              next_send_state <= fin;
            end if;

          when fin =>
            -- End condition (parity?)
            bit_send <= '1';            -- Probably not 100% correct.

            -- If we have more bytes, keep sending them, else go idle.
            if word_amt < NUM_BYTES or UNALIGN_BITS > 0 then
              next_send_state <= start;
              word_amt        <= word_amt + 1;
            else
              next_send_state <= idle;
            end if;

        end case;
    end case;
  end process;

  process (next_send_state)
  begin
    curr_send_state <= next_send_state;
  end process;

  process (next_recv_state)
  begin
    curr_recv_state <= next_recv_state;
  end process;

end UART;
