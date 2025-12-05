local wezterm = require 'wezterm'
local act = wezterm.action
local config = wezterm.config_builder()

config.freetype_load_target   = 'Light'           -- or 'Normal'
config.freetype_render_target = 'HorizontalLcd'
config.display_pixel_geometry = 'RGB'             -- or 'BGR'

-- ── per-font stacks (Terminus AA off; others AA on) ───────────────────────────
local function font_stack_terminus()
  return wezterm.font_with_fallback({
    { family = 'Terminus',               freetype_load_target = 'Mono',   freetype_render_target = 'Mono'   }, -- no AA
    { family = 'Symbols Nerd Font Mono'  },
    { family = 'DejaVu Sans Mono'        },  -- For ballot box and other symbols
    { family = 'Noto Sans Symbols 2'     },  -- Additional symbol coverage
    { family = 'Kochi Gothic',           freetype_load_target = 'Normal', freetype_render_target = 'Normal' },
    { family = 'Noto Color Emoji',       freetype_load_target = 'Normal', freetype_render_target = 'Normal' },
  })
end

local function font_stack_iosevka()
  return wezterm.font_with_fallback({
    { family = 'IosevkaTerm Nerd Font',    },
    { family = 'DejaVu Sans Mono'          },  -- For ballot box and other symbols
    { family = 'Noto Sans Symbols 2'       },  -- Additional symbol coverage
    { family = 'Kochi Gothic',             },
    { family = 'Noto Color Emoji',         },
  })
end

local function font_stack_firacode()
  return wezterm.font_with_fallback({
    { family = 'FiraCode Nerd Font',       },
    { family = 'DejaVu Sans Mono'          },  -- For ballot box and other symbols
    { family = 'Noto Sans Symbols 2'       },  -- Additional symbol coverage
    { family = 'Kochi Gothic',             },
    { family = 'Noto Color Emoji',         },
  })
end

local function font_stack_jetbrains()
  return wezterm.font_with_fallback({
    { family = 'JetBrainsMono Nerd Font',  },
    { family = 'DejaVu Sans Mono'          },  -- For ballot box and other symbols
    { family = 'Noto Sans Symbols 2'       },  -- Additional symbol coverage
    { family = 'Kochi Gothic',             },
    { family = 'Noto Color Emoji',         },
  })
end

local function font_stack_robotomono()
  return wezterm.font_with_fallback({
    { family = 'RobotoMono Nerd Font',     },
    { family = 'DejaVu Sans Mono'          },  -- For ballot box and other symbols
    { family = 'Noto Sans Symbols 2'       },  -- Additional symbol coverage
    { family = 'Kochi Gothic',             },
    { family = 'Noto Color Emoji',         },
  })
end

local function font_stack_lilex()
  return wezterm.font_with_fallback({
    { family = 'Lilex Nerd Font',          },
    { family = 'DejaVu Sans Mono'          },  -- For ballot box and other symbols
    { family = 'Noto Sans Symbols 2'       },  -- Additional symbol coverage
    { family = 'Kochi Gothic',             },
    { family = 'Noto Color Emoji',         },
  })
end

local function build_font_stack(family, size)
  -- drop the size-21 special case unless you really want it
  if family == 'terminus' then return font_stack_terminus() end
  if family == 'jetbrains' then return font_stack_jetbrains() end
  if family == 'iosevka'  then return font_stack_iosevka()  end
  if family == 'robotomono' then return font_stack_robotomono() end
  if family == 'lilex' then return font_stack_lilex() end
  if family == 'firacode' then return font_stack_firacode() end
  return font_stack_terminus()
end

-- ── defaults ──────────────────────────────────────────────────────────────────
local DEFAULT_FAMILY = 'terminus'
local DEFAULT_SIZE   = 14
config.font = build_font_stack(DEFAULT_FAMILY, DEFAULT_SIZE)
config.font_size = DEFAULT_SIZE
config.custom_block_glyphs = (DEFAULT_FAMILY == 'terminus')

-- ── load custom themes from Nix ──────────────────────────────────────────────
local custom_themes = {}
local theme_opacity = {}  -- Store opacity per theme
local theme_files = {
  dracula = 'dracula',
  light = 'light',
  gruvbox = 'gruvbox',
  dirty = 'dirty',
  twentyTwo = 'twentyTwo',
  rosePineDawn = 'rosePineDawn',
}

for name, file in pairs(theme_files) do
  local theme_path = wezterm.config_dir .. '/themes/' .. file .. '.lua'
  local ok, theme_data = pcall(dofile, theme_path)
  if ok then
    -- Extract and store opacity separately
    theme_opacity[name] = theme_data.opacity or 1.0
    -- Remove opacity from color scheme data
    theme_data.opacity = nil
    custom_themes[name] = theme_data
  end
end

-- ── appearance ─────────────────────────────────────────────────────────────────
config.color_schemes = custom_themes
config.color_scheme = 'dracula' -- default theme
config.window_background_opacity = theme_opacity['dracula'] or 1.0 -- default opacity
config.window_padding = { left = 10, right = 10, top = 10, bottom = 10 }

config.text_blink_rate = 400
config.text_blink_rate_rapid = 150
config.text_blink_ease_in = 'Constant'
config.text_blink_ease_out = 'Constant'
config.strikethrough_position = '300%' -- make strike invisible

config.hide_tab_bar_if_only_one_tab = true
config.tab_bar_at_bottom = true
config.use_fancy_tab_bar = false
config.enable_tab_bar = true

config.default_cursor_style = 'BlinkingUnderline'
config.cursor_blink_rate = 100
-- config.cursor_blink_ease_in = 'Constant'
config.cursor_blink_ease_out = 'Constant'

config.check_for_updates = false

config.scrollback_lines = 10000

-- https://wezterm.org/config/default-keys.html
-- config.disable_default_key_bindings = true
config.leader = { key="b", mods="CTRL", timeout_milliseconds=1000 }

-- ── keep state per window (WezTerm drops unknown override fields) ─────────────
local state = {} -- { [window_id] = {family=..., size=...} }

local function get_win_id(window)
  -- returns a stable identifier for the window
  return window:window_id()
end

local function get_family(window)
  local id = get_win_id(window)
  return (state[id] and state[id].family) or DEFAULT_FAMILY
end

local function get_size(window)
  local o = window:get_config_overrides() or {}
  return o.font_size or (state[get_win_id(window)] and state[get_win_id(window)].size) or DEFAULT_SIZE
end

local function set_family(window, family)
  local id = get_win_id(window)
  local size = get_size(window)
  local o = window:get_config_overrides() or {}
  o.font = build_font_stack(family, size)
  o.font_size = size
  -- I love pixelated font
  o.custom_block_glyphs = family ~= 'terminus'

  window:set_config_overrides(o)
  state[id] = { family = family, size = size }
end

local function set_size(window, size)
  local id = get_win_id(window)
  local family = get_family(window)
  local o = window:get_config_overrides() or {}
  o.font_size = size
  o.font = build_font_stack(family, size)
  window:set_config_overrides(o)
  state[id] = { family = family, size = size }
end

local function set_theme(window, theme)
  local o = window:get_config_overrides() or {}
  o.color_scheme = theme
  window:set_config_overrides(o)
end

-- track current theme per window
local theme_state = {} -- { [window_id] = theme_name }

local function get_theme(window)
  local id = get_win_id(window)
  return theme_state[id] or 'dracula'
end

local function set_theme_tracked(window, theme)
  local id = get_win_id(window)
  theme_state[id] = theme

  local o = window:get_config_overrides() or {}
  o.color_scheme = theme
  -- Apply theme's opacity
  o.window_background_opacity = theme_opacity[theme] or 1.0
  window:set_config_overrides(o)
end

-- ── action generators ─────────────────────────────────────────────────────────
local function make_family_action(family)
  return wezterm.action_callback(function(w) set_family(w, family) end)
end

local function make_size_action(size)
  return wezterm.action_callback(function(w) set_size(w, size) end)
end

local function make_theme_action(theme)
  return wezterm.action_callback(function(w) set_theme_tracked(w, theme) end)
end

-- ensure we initialize state when GUI starts
wezterm.on('gui-startup', function(cmd)
  -- nothing required here for state; it will be set on first keypress
end)

-- also update state when a window is created
wezterm.on('window-config-reloaded', function(window, _)
  local id = get_win_id(window)
  if not state[id] then
    state[id] = { family = DEFAULT_FAMILY, size = get_size(window) }
  end
end)

-- ── keys ──────────────────────────────────────────────────────────────────────
config.keys = {
  -- families
  { key = 'F1', mods = 'CTRL', action = make_family_action('terminus') },
  { key = 'F2', mods = 'CTRL', action = make_family_action('iosevka') },
  { key = 'F3', mods = 'CTRL', action = make_family_action('firacode') },
  { key = 'F4', mods = 'CTRL', action = make_family_action('jetbrains') },
  { key = 'F5', mods = 'CTRL', action = make_family_action('robotomono') },
  { key = 'F6', mods = 'CTRL', action = make_family_action('lilex') },

  -- sizes
  { key = '7', mods = 'CTRL', action = make_size_action(8) },
  { key = '8', mods = 'CTRL', action = make_size_action(10) },
  { key = '9', mods = 'CTRL', action = make_size_action(14) },
  { key = '0', mods = 'CTRL', action = make_size_action(18) },
  { key = '-', mods = 'CTRL', action = make_size_action(21) },
  { key = '=', mods = 'CTRL', action = make_size_action(24) },
  { key = 'Backspace', mods = 'CTRL', action = make_size_action(30) },

  -- themes (matching urxvt keybindings)
  { key = '1', mods = 'CTRL', action = make_theme_action('dracula') },
  { key = '2', mods = 'CTRL', action = make_theme_action('light') },
  { key = '3', mods = 'CTRL', action = make_theme_action('gruvbox') },
  { key = '4', mods = 'CTRL', action = make_theme_action('rosePineDawn') },
  { key = '5', mods = 'CTRL', action = make_theme_action('twentyTwo') },
  { key = '6', mods = 'CTRL', action = make_theme_action('dirty') },

  -- tabs
  { key = "t", mods="LEADER", action = act { SpawnTab = "CurrentPaneDomain" } },
  { key = "1", mods="LEADER", action = act { ActivateTab = 0 } },
  { key = "2", mods="LEADER", action = act { ActivateTab = 1 } },
  { key = "3", mods="LEADER", action = act { ActivateTab = 2 } },
  { key = "4", mods="LEADER", action = act { ActivateTab = 3 } },
  { key = "5", mods="LEADER", action = act { ActivateTab = 4 } },
  { key = "6", mods="LEADER", action = act { ActivateTab = 5 } },
  { key = "7", mods="LEADER", action = act { ActivateTab = 6 } },
  { key = "8", mods="LEADER", action = act { ActivateTab = 7 } },
  { key = "9", mods="LEADER", action = act { ActivateTab = 8 } },
  { key = [[\]], mods="LEADER", action = act { SplitHorizontal = { domain = "CurrentPaneDomain" } } },
  { key = [[/]], mods="LEADER", action = act { SplitVertical = { domain = "CurrentPaneDomain" } } },
  {key="h", mods="LEADER", action= act {ActivatePaneDirection="Left"}},
  {key="l", mods="LEADER", action= act {ActivatePaneDirection="Right"}},
  {key="j", mods="LEADER", action= act {ActivatePaneDirection="Down"}},
  {key="k", mods="LEADER", action= act {ActivatePaneDirection="Up"}},

  {key="UpArrow", mods="SHIFT", action= act {ScrollToPrompt=-1}},
  {key="DownArrow", mods="SHIFT", action= act {ScrollToPrompt=1}},

}

return config
