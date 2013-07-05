
function conky_fs_used_perc_pie(fs)
  percent = conky_parse("${fs_used_perc " .. fs .. "}")
  return conky_percent_pie(percent)
end

function conky_percent_pie(percent)
  percent = tonumber(percent) % 100
  
  char = math.floor(percent / 100 * 20) + 97
  
  return string.char(char)
end
