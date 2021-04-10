terraform {
  required_providers {
    linode = {
      source = "linode/linode"
      version = "1.16.0"
    }
  }
}

provider "linode" {
  token = var.linode_api_token
}

module "ci-feature-branch" {
  for_each = toset(["ubuntu20.04"])
  source = "./modules/ci-linode"
  
  authorized_keys = [var.ssh_pub_key]
  image_id = "linode/${each.key}"
}
