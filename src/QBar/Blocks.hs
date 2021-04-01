module QBar.Blocks
  ( QBar.Blocks.Battery.batteryBlock,
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
  )
where

import qualified QBar.Blocks.Battery
import qualified QBar.Blocks.CpuUsage
import qualified QBar.Blocks.Date
import qualified QBar.Blocks.DiskUsage
import qualified QBar.Blocks.NetworkManager
import qualified QBar.Blocks.Qubes
import qualified QBar.Blocks.Script
import qualified QBar.Blocks.Squeekboard
