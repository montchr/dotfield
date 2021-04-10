terraform {
  required_providers {
    linode = {
      source  = "linode/linode"
      version = "1.16.0"
    }
  }
}

provider "linode" {
  token = var.LINODE_API_TOKEN
}

module "ci-feature-branch" {
  for_each = toset(["ubuntu20.04"])
  source   = "./modules/ci-feature-branch"

  authorized_keys = [var.SSH_PUB_KEY]
  branch          = var.BRANCH
  image_id        = "linode/${each.key}"
  root_pass       = var.ROOT_PASS
}
