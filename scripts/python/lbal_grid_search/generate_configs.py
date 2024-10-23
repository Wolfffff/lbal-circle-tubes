import json
import copy

def update_backbone(config: dict, backbone: str):
    config["model"]["backbone"]["name"] = backbone

def update_filters(config: dict, number_of_filters: int, backbone: str):
    config["model"]["backbone"][backbone]["filters"] = number_of_filters

def update_max_stride(config: dict, max_stride: int, backbone: str):
    config["model"]["backbone"][backbone]["max_stride"] = max_stride

def update_sigma(config: dict, sigma: float):
    config["model"]["heads"]["centered_instance"]["sigma"] = sigma

def update_run_name(config: dict, run_name: str):
    config["outputs"]["run_name"] = run_name

# Grid to search
backbones = ["leap", "unet", "hourglass", "resnet"]
filters = [8, 16, 32, 64, 128, 256, 512]
max_strides = [2, 4, 8, 16, 32, 64]
sigmas = [2.5, 5, 7.5, 10, 12.5, 15]

# Load base configs
base_configs = []
for i in backbones:
    with open("G:/My Drive/KL/SLEAP/11_16_23_LBAL_Model_Training/lbal_grid_search/centered_instance_" + i + ".json") as file:
        base_configs.append(json.load(file))

    print("Base config loaded successfully:", i)

# Create a list of jsons
jsons = []
for b in range(len(backbones)):
    for f in filters:
        for ms in max_strides:
            for s in sigmas:
                config = copy.deepcopy(base_configs[b])
                update_backbone(config, backbones[b])
                update_filters(config, f, backbones[b])
                update_max_stride(config, ms, backbones[b])
                update_sigma(config, s)
                update_run_name(config, f"backbone_{backbones[b]}_filters_{f}_max_stride_{ms}_sigma_{s}")
                jsons.append(config)
                print(f"Generated config with backbone = {backbones[b]}, filters = {f}, max_stride = {ms}, and sigma = {s}: {config}")

for config in jsons:
    file_path = f"G:/My Drive/KL/SLEAP/11_16_23_LBAL_Model_Training/lbal_grid_search/new_configs/config_{config['outputs']['run_name']}.json"

    try:
        with open(file_path, "w") as f:
            json.dump(config, f, indent = 4)
            print(f"Writing {f.name}")

    except IOError as e:
        print(f"Error writing file {file_path}: {e}")