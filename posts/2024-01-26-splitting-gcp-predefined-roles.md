---
title: Splitting predefined GCP roles
tags: howto, gcp, google cloud, terraform
excerpt: |
  A short sketch of automatically creating custom roles that split the
  permissions in a pre-defined GCP role based on a regex. This can be useful
  in an environment which requires, e.g., more control over permissions like
  Set IAM Policy.
---

Recently I've worked with a client who wanted to restrict access to any *Set IAM
Policy* permissions in their Google Cloud Platform environment. Currently this
is implemented by defining a small number of custom roles which fall into one of
two groups:

- Roles which include all the *Set IAM Policy* permissions for the necessary
  services;

- Role/s which include all the non-*Set IAM Policy* permissions for the
  necessary services.

Currently these are defined by explicitly listing all the permissions that
should be granted (as far as they are known at the time we edit the definition).
I've recently been thinking about an approach that might help move toward a more
manageable approach.

1. Use the Google pre-defined roles where possible. This will help make sure
   that documentation, error message, examples, etc. is more directly applicable
   to the client's environment.

2. Some pre-defined roles can't be used because they mix *Set IAM Policy* with
   other permissions that the client wants to manage separately. In this case,
   use Terraform to define custom roles, but do so based on the definition of
   the pre-defined role.

Here's a sketch of what this might look like.


```{.terraform}
variable "target_role" {
  type        = string
  description = "ID of the target role."
}

# Fetch the existing role.
data "google_iam_role" "role" {
  name = var.target_role
}

locals {
  role_components = split("/", var.target_role)
  role_name       = element(local.role_components, length(local.role_components) - 1)

  # Every permission in the target role that **IS** a setIamPolicy permission.
  setiam_permissions = [
    for permission in data.google_iam_role.role.included_permissions:
    permission if length(regexall("^.*[.]setIamPolicy$", permission)) == 1
  ]

  # Every permission in the target role that **IS NOT** a setIamPolicy permission.
  normal_permissions = [
    for permission in data.google_iam_role.role.included_permissions:
    permission if length(regexall("^.*[.]setIamPolicy$", permission)) == 0
  ]
}

resource "google_project_iam_custom_role" "nonpriv_role" {
  role_id     = "custom.${role_name}.nonpriv"
  title       = "${data.google_iam_role.role.title} - (Non-priv)"
  description = "(Custom non-privileged version) ${data.google_iam_role.role.description}."
  permissions = local.normal_permissions
}

resource "google_project_iam_custom_role" "priv_role" {
  role_id     = "custom.${role_name}.priv"
  title       = "${data.google_iam_role.role.title} - (Priv)"
  description = "(Custom privileged version) ${data.google_iam_role.role.description}."
  permissions = local.setiam_permissions
}

output "permissions_role" {
    value  = resource.google_project_iam_custom_role.nonpriv_role
}

output "set_iam_policy_role" {
    value  = resource.google_project_iam_custom_role.priv_role
}
```

