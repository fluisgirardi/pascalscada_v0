# 变更说明：台达驱动站号 0 严格化校验

- **日期**：2026-11-24
- **组件**：`TDeltaDriver` / `TDeltaTCPDriver`（台达 DVP / AS 系列，Modbus TCP）
- **变更类型**：缺陷修复（读写路径校验不一致）
- **处理方式**：用户选择「严格化」——让写路径与读路径采用相同的站号校验，非法配置直接报错，不再出现“能写不能读”的单向通讯。

---

## 1. 问题现象

使用台达驱动（`Series = dvsAS`）时：

- `TPLCTagNumber.PLCStation = 0`，PLC 实际站号为 1：
  - 写数据：PLC 侧能收到并正确执行；
  - 读数据：上位机读不到，标签值不更新。
- 将 `PLCStation` 改为 1 后：读写均正常。

按常理站号不匹配应当读写都失败，因此该现象容易被误判为 PLC 或通讯链路问题。

## 2. 根因分析

问题不是 PLC 行为异常，而是驱动内部读、写两条路径对站号 0 的处理不一致。

### 2.1 读路径：站号 0 被拒绝注册

- `DeltaDriver.pas:731` 的 `DoAddTag` 只接受 `station in [1..255]`。
- 站号为 0 的标签不会加入 `PDeltaPLC` 扫描表，`DoScanRead`（`DeltaDriver.pas:1206` 起）遍历不到该标签，因此**根本不会发出读报文**。
- 即使直接调用读接口，`DoGetValue`（`DeltaDriver.pas:817` 附近）按站号在 `PDeltaPLC` 中查找，找不到时直接返回 `ioDriverError` 并清空值。

### 2.2 写路径：站号未校验，报文直接发出

- `DoWrite` → `WriteTagRec` → `SendWrite` → `EncodePkg` 链路没有站号范围检查，也不依赖 `DoAddTag` 的注册结果。
- `DeltaTCP.pas:168 / 199 / 221 / 247` 直接把 `TagObj.Station`（这里是 0）填入 Modbus TCP MBAP 头的 unit id 字节。
- 台达 AS 系列对 unit id 不严格校验，因此 unit id = 0 的写请求仍被 PLC 接受并执行，造成“写成功”的假象。

### 2.3 写响应也不回写缓存

- `DeltaTCP.pas:342` 的 `DecodePkg` 用响应中的 unit id 查找 `PDeltaPLC`；站号 0 找不到（`foundPLC = false`），写响应不会调用 `SetValues` 回写驱动缓存。
- 因此即使写成功，上位机缓存也不会被这次写入更新。

---

## 3. 变更内容

修改文件：`src/scada/DeltaDriver.pas`

在读、写两个 I/O 入口增加完全一致的站号合法性校验，使用枚举中已有的 `ioIllegalStationAddress` 错误码。

**`DoRead`（修改后 `DeltaDriver.pas:1171-1188`）：**

```pascal
tr := tagrec;
SetLength(Values,0);

// Modbus unit id 0 is not a valid single-slave address. Reject it on every
// I/O path so a misconfigured station is reported instead of silently
// producing a one-way (write-only) communication.
if not (tr.Station in [1..255]) then begin
  Result := ioIllegalStationAddress;
  exit;
end;

NormalizeTagRec(tr);
Result := ReadTagRec(tr,Values);
```

**`DoWrite`（修改后 `DeltaDriver.pas:1190-1204`）：**

```pascal
tr := tagrec;

// See DoRead: keep read and write validation identical.
if not (tr.Station in [1..255]) then begin
  Result := ioIllegalStationAddress;
  exit;
end;

NormalizeTagRec(tr);
Result := WriteTagRec(tr,Values);
```

要点：

1. 校验放在 `NormalizeTagRec` 之前，非法站号不会触发任何地址换算或报文组包。
2. 读路径清空输出值，避免返回上一次的残留数据。
3. 合法站号（1~255）的流程完全不变。

---

## 4. 验证结果

### 4.1 编译验证

| 目录 | 命令 | 结果 |
| --- | --- | --- |
| 主目录 `E:\AI软件制作\Pascalscada\pascalscada_v0-master` | `lazbuild --recursive pascalscada.lpk` | 通过（仅原有 Hint） |
| D 盘副本 `D:\lazarus\components\pascalscada最新控件\pascalscada_v0-master` | `lazbuild -B --recursive pascalscada.lpk` | 通过（仅原有 Hint） |

### 4.2 行为验证

用 `TDeltaTCPDriver` 的测试子类直接调用 `DoRead` / `DoWrite`（测试程序放在临时目录 `D:\mqttbuild\testdelta.pas`，不入库）：

| 站号 | 调用 | 返回码 | 说明 |
| --- | --- | --- | --- |
| 0 | `DoWrite` | 12 `ioIllegalStationAddress` | 被拒绝，未发报文 |
| 0 | `DoRead` | 12 `ioIllegalStationAddress` | 被拒绝，未发报文 |
| 1 | `DoWrite` | 10 `ioNullDriver` | 已通过站号校验，因测试环境未挂通讯端口而在后续流程返回 |
| 1 | `DoRead` | 10 `ioNullDriver` | 同上，合法站号未受影响 |

（枚举序号：`ioNone=0, ioOk=1, ioDriverError=2, ioCommError=3, ioTimeOut=4, ioIllegalFunction=5, ioIllegalRegAddress=6, ioIllegalValue=7, ioPLCError=8, ioTagError=9, ioNullDriver=10, ioIllegalRequest=11, ioIllegalStationAddress=12`）

### 4.3 文件一致性

主目录与 D 盘副本的 `DeltaDriver.pas` 已 `cmp` 校验一致，符合双目录同步约定。

---

## 5. 影响范围与兼容性

- **行为变化**：站号填 0 时，读写都会立即返回 `ioIllegalStationAddress`，不再出现“写通、读不通”。该错误会按驱动原有机制上报到标签/日志。
- **不受影响**：站号 1~255 的标签，读写流程、报文格式、缓存行为均保持原样。
- **配置建议**：`PLCStation` 必须填写 PLC 实际站号（本例为 1）。
- **部署注意**：
  - 已编译的应用程序需要**重新编译**才会使用新代码（D 盘运行期包的 `.ppu` 已更新）。
  - `pascalscada` / `pascalscada_dsng` 是静态链接进 Lazarus IDE 的包；若希望 IDE 设计期行为也同步更新，需要重建 IDE 并重启（可复用此前建立的 ASCII 路径方案 `D:\laz_ascii`）。

## 6. 涉及文件

| 文件 | 变更 |
| --- | --- |
| `E:\AI软件制作\Pascalscada\pascalscada_v0-master\src\scada\DeltaDriver.pas` | 新增读写站号校验 |
| `D:\lazarus\components\pascalscada最新控件\pascalscada_v0-master\src\scada\DeltaDriver.pas` | 同步副本 |
| `E:\AI软件制作\Pascalscada\pascalscada_v0-master\docs\changelog-delta-station-strict-2026-11-24.md` | 本变更说明 |
| `D:\lazarus\components\pascalscada最新控件\pascalscada_v0-master\docs\changelog-delta-station-strict-2026-11-24.md` | 本变更说明副本 |

## 7. 未采纳的替代方案（备查）

若将来确实需要支持站号 0，则应采用“放宽”而非“严格化”，并且必须同时修改：

1. `DoAddTag` / `DoDelTag` 的范围判断 `station in [1..255]` → `0..255`；
2. `DoGetValue` 按站号查找 `PDeltaPLC` 的逻辑；
3. `DeltaTCP.pas:342` 的 `foundPLC` 匹配逻辑，使写响应能回写缓存。

单独放宽写路径正是本次问题的成因，不应只改一处。
