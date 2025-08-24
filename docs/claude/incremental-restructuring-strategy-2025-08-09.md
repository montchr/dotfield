# Incremental Dotfiles Restructuring Strategy

**Date:** August 9, 2025  
**Context:** Response to analysis of architectural instability patterns  
**Goal:** Provide structured approach to reduce configuration churn and instability

Based on the analysis of your project's instability patterns, here's how you could approach a more incremental restructuring strategy:

## **Phase-Based Incremental Approach**

### **Phase 1: Stabilization Foundation (Weeks 1-2)**
**Goal**: Stop the bleeding on high-churn areas

1. **Freeze experimental features**
   - Document current state of unstable areas (Tuuvok, overlays, themes)
   - Create `experimental/` branches for new features instead of main integration
   - Establish "stable core" vs "experimental additions" boundaries

2. **Create rollback safety nets**
   - Add `just rollback` command using `nixos-rebuild --rollback`
   - Document working configurations before changes
   - Create host-specific "last known good" tags

### **Phase 2: Modularization (Weeks 3-4)**
**Goal**: Break apart tightly-coupled configurations

1. **Extract host-specific complexity**
   ```nix
   # Instead of inline conditionals everywhere
   src/hosts/tuuvok/overrides.nix  # Apple Silicon specific
   src/hosts/stable/common.nix     # Conservative defaults
   ```

2. **Create feature toggles**
   ```nix
   dotfield.aspects = {
     experimental-wayland = false;  # Default safe
     bleeding-edge-packages = false;
     ui-customizations = "minimal";  # vs "full"
   };
   ```

### **Phase 3: Dependency Isolation (Weeks 5-6)**
**Goal**: Prevent cascade failures

1. **Separate overlay strategies**
   ```nix
   overlays = {
     stable = [ /* core overlays only */ ];
     experimental = stable ++ [ nixpkgs-wayland ];
   };
   ```

2. **Isolate integration points**
   - Git tools: Make delta/difftastic swappable without config changes
   - Emacs: Separate platform-specific vs universal config
   - Firefox: Extract theme as optional module

### **Phase 4: Incremental Migration (Weeks 7-8)**
**Goal**: Apply lessons learned to reduce future churn

1. **Create migration utilities**
   ```bash
   just migrate-host <old> <new>  # Template from working config
   just test-feature <feature>    # Isolated testing
   just promote-feature <feature> # Move experimental→stable
   ```

2. **Establish change protocols**
   - All experimental features start in branches
   - Changes require explicit stability assessment
   - Document decision rationale in commit messages

## **Specific Instability Fixes**

### **Tuuvok (Apple Silicon) Stabilization**
```nix
# src/hosts/tuuvok/conservative-profile.nix
{
  # Disable problematic features by default
  dotfield.hardware.pro-audio = false;
  dotfield.graphics.experimental-mesa = false;
  
  # Provide override mechanism
  dotfield.overrides.enable-experimental = false;
}
```

### **Overlay Management Strategy**
```nix
# src/modules/overlays/strategy.nix
{ config, ... }: {
  options.dotfield.overlay-strategy = mkOption {
    type = types.enum [ "stable" "balanced" "bleeding-edge" ];
    default = "stable";
  };
  
  config = mkMerge [
    (mkIf (config.dotfield.overlay-strategy == "stable") {
      nixpkgs.overlays = [ /* minimal set */ ];
    })
    # ... other strategies
  ];
}
```

### **Authentication Safety**
```nix
# Always keep emergency user access
users.users.root.hashedPassword = "!";  # Locked but can unlock
security.sudo.wheelNeedsPassword = mkDefault false;  # Emergency access

# Rollback mechanism for auth changes
systemd.services.auth-rollback = {
  # Timer to restore previous auth state if system unreachable
};
```

## **Development Workflow Changes**

### **Branch Strategy**
```bash
main              # Stable, working configs only
experimental/     # New features, can be broken
staging/         # Integration testing
archive/         # Failed experiments for reference
```

### **Testing Protocol**
```bash
# Before any auth/boot changes
just backup-current-config
just test-build-all-hosts
just create-recovery-media

# For experimental features
just test-in-vm <feature>
just test-on-non-critical-host
```

## **Decision Framework**

### **Before Adding Any Feature**
1. **Categorize**: Core vs Optional vs Experimental
2. **Assess maintenance burden**: High/Medium/Low
3. **Plan exit strategy**: How to cleanly remove if needed
4. **Document integration points**: What breaks if this fails?

### **Change Approval Checklist**
- [ ] Can be cleanly reverted?
- [ ] Won't break other hosts?
- [ ] Has been tested in isolation?
- [ ] Documentation updated?
- [ ] Maintenance cost acceptable?

## **Implementation Priority**

### **High Impact, Low Risk Changes**
1. **Create feature toggles** for known unstable areas
2. **Extract Tuuvok-specific overrides** to isolated modules
3. **Add rollback utilities** to justfile
4. **Document current working state** before any changes

### **Medium Priority**
1. **Overlay strategy refactoring** - affects build times but manageable
2. **Authentication safety mechanisms** - critical but requires careful testing
3. **Branch workflow establishment** - changes development process

### **Future Considerations**
1. **Full modularization** - major architectural change
2. **Migration tooling** - nice-to-have utilities
3. **Automated testing** - requires infrastructure setup

## **Success Metrics**

### **Stability Indicators**
- Reduced "again" commits (currently 15+ instances)
- Fewer reverts (currently 15+ explicit reverts)
- Stable Tuuvok host configuration (currently most unstable)
- Consistent overlay strategy (currently 6+ flip-flops)

### **Development Velocity**
- Time from feature idea to stable integration
- Reduced time spent on configuration debugging
- Faster rollback/recovery from failed changes
- Clear decision making on experimental features

## **Root Cause Mitigation**

This approach would have prevented many of the flip-flops by:

- **Staging experimental features** instead of main integration
- **Isolating failure points** so one broken feature doesn't cascade  
- **Creating explicit decision points** with documented trade-offs
- **Maintaining escape hatches** for quick rollbacks

## **Key Insight**

The analysis reveals you need **structured experimentation** rather than avoiding experimentation entirely. Your early adopter instincts are valuable - the issue is the lack of containment and rollback mechanisms when experiments don't work out.

The goal is to maintain your innovative edge while building the safety nets that prevent experimental features from destabilizing your core daily-use configurations.

---

*This strategy builds on the patterns identified in the dotfiles history analysis, focusing on the specific areas of highest instability while preserving the experimental nature that drives innovation in the configuration.*