# Revision history for `clash-protocols`

## 0.1 -- 28-04-2026

* First release

## 0.1.1 -- 29-04-2026

* Fix link in package description

## Unreleased

* Add `Protocols.Experimental.Testbench`, an experimental testbench-based alternative to `Protocols.Experimental.Hedgehog`. Testbenches are built from protocol-specific drivers, sinks, monitors, scoreboards, and cover points, and can write waveforms. Components for `Df` are in `Protocols.Experimental.Testbench.Df`. See [#144](https://github.com/clash-lang/clash-protocols/issues/144).
* Define the `Protocol` instances for `()`, tuples, `Vec`, `CSignal`, `Clock`, `DiffClock`, `Reset`, and `Enable` in `Protocols.Plugin.Types`, so they are no longer orphan instances.
