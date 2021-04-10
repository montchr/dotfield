locals {
  safe_label = replace(
    lower(
      format(
        "ci-dots-%s-%s",
        var.branch,
        replace(var.image_id, "/(linode)\\W/", "")
      ),
    ),
    "/[[:punct:]]/",
    "-"
  )
}

resource "linode_instance" "ci-feature-branch" {
  name            = local.safe_label
  image           = var.image_id
  label           = local.safe_label
  group           = "Dotfield CI"
  region          = var.region
  type            = "g6-nanode-1"
  authorized_keys = var.authorized_keys
  root_pass       = var.root_pass
}
