library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity UART_Control is
  generic (
    WORD_SZ : natural;
    );
  port(
    clock    : in  std_logic;
    word     : in  unsigned(WORD_SZ-1 downto 0);
    data_out : out std_logic
    );
end entity;

architecture UART_Control of UART_Control is
  component uart is
    port(
      clock    : in  std_logic;
      byte_in  : out std_logic_vector(7 downto 0);
      byte_out : in  std_logic_vector(7 downto 0);
      bit_out  : out std_logic;
      bit_in   : in  std_logic;
      transmit : in  std_logic
      );
  end component;

  type state_t is (idle, init, fire, err);  --, done, reset);

  signal state, next_state : state_t              := idle;
  signal transmit          : std_logic;
  signal transmit_buf      : unsigned(WORD_SZ-1 downto 0);
  signal send_byte         : unsigned(7 downto 0) := (others => '0');
  signal can_send          : std_logic            := '0';

  -- UART stuff (Have to ignore some of Tyler's crap)
  signal ignore_vec : std_logic_vector(7 downto 0);
  signal ignore_bit : std_logic;

  constant MAX_ROTATE : natural := WORD_SZ / send_byte'length;
begin

  uart_out : uart port map(clock, ignore_vec, send_byte, data_out, ignore_bit, transmit);

  process (clock)

  begin
    if rising_edge(clock) then
      state <= next_state;
    end if;
  end process;

  -- This will probably generate latches (might be okay though)
  process (state, transmit_buf, word, send_byte)
    variable curr_rotate : natural := 0;
  begin
    transmit   <= '0';
    next_state <= idle;

    case state is
      when idle =>
        -- Try to start the process
        if transmit_buf \= word then
          next_state <= init;
        end if;

      when init =>
        -- Copy buffer into transmit buffer.
        transmit_buf <= word;
        curr_rotate  := 0;
        next_state   <= fire;

      when fire =>
        transmit <= not(transmit);
        if transmit then
          if curr_rotate < MAX_ROTATE then
            -- Rotate the number of times we need, based on how much to send.
            send_byte    <= transmit_buf(send_byte'left downto send_byte'right);
            transmit_buf <= transmit_buf ror send_byte'length;
            curr_rotate  := curr_rotate + 1;
          else
            -- We're done transmitting
            next_state <= idle;
          end if;
        end if;

      when err =>
        transmit     <= 'U';
        transmit_buf <= (others => 'U');

      when others =>
        next_state <= err;
    end process;

  end architecture;
