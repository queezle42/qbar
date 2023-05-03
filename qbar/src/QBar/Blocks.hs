module QBar.Blocks (
  QBar.Blocks.Battery.batteryBlock,
  QBar.Blocks.CpuUsage.cpuUsageBlock,
  QBar.Blocks.Date.dateBlock,
  QBar.Blocks.DiskUsage.diskUsageBlock,
  QBar.Blocks.NetworkManager.networkManagerBlock,
  QBar.Blocks.Qubes.diskUsageQubesBlock,
  QBar.Blocks.Qubes.qubesMonitorPropertyBlock,
  QBar.Blocks.Qubes.qubesVMCountBlock,
  QBar.Blocks.Script.scriptBlock,
  QBar.Blocks.Script.pollScriptBlock,
  QBar.Blocks.Squeekboard.squeekboardBlock,
) where

import QBar.Blocks.Battery qualified
import QBar.Blocks.CpuUsage qualified
import QBar.Blocks.Date qualified
import QBar.Blocks.DiskUsage qualified
import QBar.Blocks.NetworkManager qualified
import QBar.Blocks.Qubes qualified
import QBar.Blocks.Script qualified
import QBar.Blocks.Squeekboard qualified
