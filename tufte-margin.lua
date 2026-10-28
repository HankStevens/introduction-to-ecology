-- tufte-margin.lua ----
-- PDF only. Quarto renders free-standing `::: {.column-margin}` notes with
-- \marginnote, which does not avoid collisions, so notes can print on top
-- of sidenotes and margin citations. This re-routes plain-text margin notes
-- through \marginpar, which marginfix shifts out of the way.
-- Margin figures, tables, and code cells are left to Quarto.

if not quarto.doc.is_format("latex") then return {} end

local function has_float(div)
  local found = false
  div:walk({
    Image = function() found = true end,
    Figure = function() found = true end,
    Table = function() found = true end,
    CodeBlock = function() found = true end,
  })
  return found
end

function Div(div)
  if div.classes:includes("column-margin")
     and not div.classes:includes("cell")
     and not has_float(div) then
    local out = pandoc.List({ pandoc.RawBlock("latex", "\\marginpar{\\footnotesize\\raggedright") })
    out:extend(div.content)
    out:insert(pandoc.RawBlock("latex", "}"))
    return out
  end
end
